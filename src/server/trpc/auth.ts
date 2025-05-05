import { router, loggedInProcedure, anonymousProcedure } from ".";
import { z } from "zod";
import { users } from "@/server/schema";
import { eq } from "drizzle-orm";
import {
  invalidateAllSessions,
  hashPassword,
  generateSessionToken,
  createSession,
  verifyPasswordStrength,
  verifyPasswordHash,
} from "@/server/auth";
import { TRPCError } from "@trpc/server";

// shared between signup and login
const signinProcedure = anonymousProcedure.input(
  z.object({
    username: z.string().min(8),
    password: z.string().min(8),
  }),
);

export const authRouter = router({
  me: anonymousProcedure.query(({ ctx }) => {
    return { userId: ctx.userId };
  }),
  logout: loggedInProcedure.mutation(
    async ({ ctx: { userId, responseHeaders } }) => {
      // expire cookie
      responseHeaders.set(
        "Set-Cookie",
        "session=; Path=/; Expires=Thu, 01 Jan 1970 00:00:00 GMT; HttpOnly",
      );
      if (!userId) return;
      // clear database entry
      await invalidateAllSessions(userId);
    },
  ),

  signup: signinProcedure.mutation(
    async ({ input, ctx: { db, responseHeaders } }) => {
      // Check if username already exists
      const existingUser = await db
        .select()
        .from(users)
        .where(eq(users.username, input.username))
        .get();

      if (existingUser)
        throw new TRPCError({
          code: "BAD_REQUEST",
          message: "Username is taken",
        });

      // Verify password strength
      const isStrongPassword = await verifyPasswordStrength(input.password);
      if (!isStrongPassword) {
        throw new TRPCError({
          code: "BAD_REQUEST",
          message: "Password is too weak or has been compromised",
        });
      }

      // Hash password and create user
      const hashedPassword = await hashPassword(input.password);
      const result = await db
        .insert(users)
        .values({
          username: input.username,
          hashedPassword,
        })
        .returning()
        .get();

      // Create session
      const token = generateSessionToken();
      await createSession(token, result.id);

      // Set session cookie
      responseHeaders.set(
        "Set-Cookie",
        `session=${token}; Path=/; HttpOnly; SameSite=Lax`,
      );
      responseHeaders.set("Location", "/lobby");

      return { userId: result.id };
    },
  ),

  login: signinProcedure.mutation(
    async ({ input, ctx: { db, responseHeaders } }) => {
      // Find user
      const user = await db
        .select()
        .from(users)
        .where(eq(users.username, input.username))
        .get();

      if (!user) {
        throw new TRPCError({
          code: "BAD_REQUEST",
          message: "Invalid username or password",
        });
      }

      // Verify password
      const isValidPassword = await verifyPasswordHash(
        user.hashedPassword,
        input.password,
      );

      if (!isValidPassword)
        throw new TRPCError({
          code: "BAD_REQUEST",
          message: "Invalid username or password",
        });

      // Create session
      const token = generateSessionToken();
      await createSession(token, user.id);

      // Set session cookie
      responseHeaders.set(
        "Set-Cookie",
        `session=${token}; Path=/; HttpOnly; SameSite=Lax`,
      );
      responseHeaders.set("Location", "/lobby");
      return { userId: user.id };
    },
  ),
});
