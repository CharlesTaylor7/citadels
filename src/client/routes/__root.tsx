import {
  Link,
  Outlet,
  createRootRouteWithContext,
  useNavigate,
} from "@tanstack/react-router";
import { TanStackRouterDevtools } from "@tanstack/react-router-devtools";
import { ReactQueryDevtools } from "@tanstack/react-query-devtools";
import { useMutation, useQuery } from "@tanstack/react-query";
import { trpc } from "@/client/router";
import type { TRPCOptionsProxy } from "@trpc/tanstack-react-query";
import type { AppRouter } from "@/server/trpc/router";
import type { QueryClient } from "@tanstack/react-query";

export interface RouterAppContext {
  trpc: TRPCOptionsProxy<AppRouter>;
  queryClient: QueryClient;
}

export const Route = createRootRouteWithContext<RouterAppContext>()({
  component: RootComponent,
  loader: async ({ context: { trpc, queryClient } }) => {
    await queryClient.ensureQueryData(trpc.auth.me.queryOptions());
  },
});

function RootComponent() {
  const userQuery = useQuery(trpc.auth.me.queryOptions());
  const logoutMutation = useMutation(trpc.auth.logout.mutationOptions());
  const navigate = useNavigate();
  async function logout() {
    await logoutMutation.mutateAsync();
    navigate({ to: "/" });
  }
  const user = userQuery.data?.userId;

  return (
    <div className="min-h-screen flex flex-col bg-base-100">
      <nav className="navbar bg-base-200 shadow-lg">
        <div className="container mx-auto">
          <div className="flex-none gap-2">
            <ul className="menu menu-horizontal px-1 gap-2 items-center">
              <li>
                <Link to="/" className="btn btn-ghost text-xl">
                  Citadels
                </Link>
              </li>
              <li>
                <Link
                  to="/lobby"
                  className="btn btn-ghost btn-sm"
                  activeProps={{ className: "btn-outline" }}
                >
                  Lobby
                </Link>
              </li>
              <li>
                <Link
                  to="/game"
                  className="btn btn-ghost btn-sm"
                  activeProps={{ className: "btn-outline" }}
                >
                  Game
                </Link>
              </li>
              {user ? (
                <li className="dropdown dropdown-hover">
                  <label tabIndex={0} className="btn btn-ghost btn-sm">
                    Account
                  </label>
                  <ul
                    tabIndex={0}
                    className="dropdown-content menu p-2 shadow bg-base-100 rounded-box w-52"
                  >
                    <li>
                      <button
                        onClick={() => logout()}
                        className="btn btn-ghost text-error"
                      >
                        Logout
                      </button>
                    </li>
                  </ul>
                </li>
              ) : (
                <li>
                  <Link
                    to="/login"
                    className="btn btn-ghost btn-sm"
                    activeProps={{ className: "btn-outline" }}
                  >
                    Login
                  </Link>
                </li>
              )}
            </ul>
          </div>
        </div>
      </nav>

      <main className="flex-1 container mx-auto">
        <Outlet />
      </main>

      <TanStackRouterDevtools />
      <ReactQueryDevtools />
    </div>
  );
}
