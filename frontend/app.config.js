import { TanStackRouterVite } from "@tanstack/router-plugin/vite";
import { createApp } from "vinxi";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";
import tsconfigPaths from "vite-tsconfig-paths";
import { visualizer } from "rollup-plugin-visualizer";

export default createApp({
  server: {
    preset: "node_server",
    experimental: {
      asyncContext: true,
    },
  },
  routers: [
    {
      type: "static",
      name: "public",
      dir: "./public",
    },
    {
      type: "http",
      name: "trpc",
      base: "/trpc",
      handler: "./src/server/main.ts",
      target: "server",
      plugins: () => [tsconfigPaths()],
    },
    {
      type: "spa",
      name: "client",
      handler: "./index.html",
      target: "browser",
      plugins: () => [
        TanStackRouterVite({
          target: "react",
          autoCodeSplitting: true,
          routesDirectory: "./src/client/routes",
          generatedRouteTree: "./src/client/route-tree.ts",
          enableRouteTreeFormatting: false,
          enableRouteGeneration: true,
        }),
        react(),
        tailwindcss(),
        tsconfigPaths(),
        visualizer({ filename: "stats/rollup.html" }),
      ],
    },
  ],
});
