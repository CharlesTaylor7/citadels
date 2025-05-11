import { router } from "./index";
import { lobbyRouter } from "./lobby";
import { gameRouter } from "./game";
import { authRouter } from "./auth";

// Merge all routers
export const appRouter = router({
  auth: authRouter,
  lobby: lobbyRouter,
  game: gameRouter,
});

export type AppRouter = typeof appRouter;
