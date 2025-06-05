import type { QueryClient } from "@tanstack/react-query";
import { Outlet, createRootRouteWithContext } from "@tanstack/react-router";
import { TanStackRouterDevtools } from "@tanstack/react-router-devtools";
import { ReactQueryDevtools } from "@tanstack/react-query-devtools";
import { Toaster } from "sonner";
import { Navbar } from "@/components/Navbar";

export interface RouterAppContext {
  queryClient: QueryClient;
}

export const Route = createRootRouteWithContext<RouterAppContext>()({
  component: RootComponent,
  head: () => ({
    meta: [
      {
        name: "description",
        content: "Play Citadels in browser",
      },
    ],
  }),
});

function RootComponent() {
  return (
    <div className="min-h-screen flex flex-col bg-base-100">
      <Navbar />
      <Outlet />
      <Toaster />
      <TanStackRouterDevtools />
      <ReactQueryDevtools />
    </div>
  );
}
