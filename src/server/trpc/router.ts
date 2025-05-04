import { router } from "./index";
import { lobbyRouter } from "./lobby";
import { gameRouter } from "./game";
import { authRouter } from "./auth";
import { assetRouter } from "./asset";

// Merge all routers
export const appRouter = router({
  auth: authRouter,
  lobby: lobbyRouter,
  game: gameRouter,
  asset: assetRouter,
});

export type AppRouter = typeof appRouter;
