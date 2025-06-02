import { validateSessionToken } from "@/server/auth";
import { FetchCreateContextFnOptions } from "@trpc/server/adapters/fetch";
import { connect } from "@/server/db";

export type Context = {
  request: Request;
  responseHeaders: Headers;
  userId: number | null;
  db: Awaited<ReturnType<typeof connect>>;
};

export async function createContext({
  req,
  resHeaders,
}: FetchCreateContextFnOptions): Promise<Context> {
  const db = await connect();
  const userId = await getUserSession(req);
  return { db, userId, request: req, responseHeaders: resHeaders };
}

export async function getUserSession(
  req: FetchCreateContextFnOptions["req"],
): Promise<number | null> {
  const sessionCookie = req.headers.get("cookie");
  const sessionToken = sessionCookie ? sessionCookie.split("=")[1] : "";
  const user = await validateSessionToken(sessionToken);
  return user;
}
