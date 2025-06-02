import { initTRPC, TRPCError } from "@trpc/server";
import { Context } from "@/server/context";

const trpc = initTRPC.context<Context>().create({
  // log errors in server
  errorFormatter({ shape }) {
    if (shape?.data?.stack) {
      console.error(shape.data.stack);
    } else {
      console.error(shape?.message);
    }
    // only send back error message
    return shape.message;
  },
});
export const router = trpc.router;
export const anonymousProcedure = trpc.procedure;
export const loggedInProcedure = trpc.procedure.use((opts) => {
  if (!opts.ctx.userId) {
    console.log(opts);
    throw new TRPCError({
      code: "UNAUTHORIZED",
      message: "User not authenticated",
    });
  }
  return opts.next({ ctx: { ...opts.ctx, userId: opts.ctx.userId } });
});
