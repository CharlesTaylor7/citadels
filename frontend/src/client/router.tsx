import { createRouter as createTanStackRouter } from "@tanstack/react-router";
import {
  QueryCache,
  QueryClient,
  QueryClientProvider,
} from "@tanstack/react-query";
import {
  splitLink,
  createTRPCClient,
  httpBatchLink,
  httpSubscriptionLink,
} from "@trpc/client";
import { createTRPCOptionsProxy } from "@trpc/tanstack-react-query";
import { routeTree } from "@/client/route-tree";
import type { AppRouter } from "@/server/trpc/router";
import { toast } from "react-hot-toast";

export const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      // 5 minutes
      staleTime: 5 * 60 * 1000,
      refetchOnWindowFocus: true,
      refetchOnMount: true,
    },
  },
  queryCache: new QueryCache({
    onError: (error) => {
      console.error(error);
      toast.error(error.message);
    },
  }),
});

const serverLink = splitLink<AppRouter>({
  condition: (op) => op.type === "subscription",
  true: httpSubscriptionLink({ url: "/trpc" }),
  false: httpBatchLink({ url: "/trpc" }),
});
export const trpc = createTRPCOptionsProxy<AppRouter>({
  client: createTRPCClient({ links: [serverLink] }),
  queryClient,
});

export function createRouter() {
  const router = createTanStackRouter({
    routeTree,
    scrollRestoration: true,
    defaultPreload: "intent",
    context: {
      trpc,
      queryClient,
    },
    defaultPendingComponent: () => <div className={`p-2 text-2xl`}></div>,
    Wrap: function WrapComponent({ children }) {
      return (
        <QueryClientProvider client={queryClient}>
          {children}
        </QueryClientProvider>
      );
    },
  });

  return router;
}

// Register the router instance for type safety
declare module "@tanstack/react-router" {
  interface Register {
    router: ReturnType<typeof createRouter>;
  }
}
