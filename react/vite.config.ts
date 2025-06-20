import { defineConfig } from "vite";
import { TanStackRouterVite } from "@tanstack/router-plugin/vite";
import react from "@vitejs/plugin-react";
import tailwindcss from "@tailwindcss/vite";
import tsconfigPaths from "vite-tsconfig-paths";
import { visualizer } from "rollup-plugin-visualizer";

export default defineConfig({
  plugins: [
    TanStackRouterVite({
      target: "react",
      autoCodeSplitting: true,
      routesDirectory: "./src/routes",
      generatedRouteTree: "./src/route-tree.ts",
      enableRouteTreeFormatting: false,
      enableRouteGeneration: true,
    }),
    react(),
    tailwindcss(),
    tsconfigPaths(),
    visualizer({ filename: "stats/rollup.html" }),
  ],
  build: {
    outDir: "../public/react/",
    emptyOutDir: true,
  },
  server: {
    open: true,
    port: 3000,
    proxy: {
      "/api": {
        target: "http://localhost:8080",
      },
      "/static": {
        target: "http://localhost:8080",
      },
    },
  },
});
