import { Toaster } from "sonner";
import {
  Link,
  Outlet,
  createRootRouteWithContext,
  useNavigate,
} from "@tanstack/react-router";
import { TanStackRouterDevtools } from "@tanstack/react-router-devtools";
import { ReactQueryDevtools } from "@tanstack/react-query-devtools";
import type { QueryClient } from "@tanstack/react-query";
import { api } from "@/api";

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
    // emoji favicon
    links: [
      {
        rel: "icon",
        href: "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🔥</text></svg>",
      },
    ],
  }),
});

function RootComponent() {
  const navigate = useNavigate();
  const userQuery = api.useQuery("get", "/auth/me", {});
  const logoutMutation = api.useMutation("post", "/auth/logout", {
    onSuccess: () => navigate({ to: "/" }),
  });

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
              {userQuery.data ? (
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
                        onClick={() => logoutMutation.mutate({})}
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

      <Toaster />
      <TanStackRouterDevtools />
      <ReactQueryDevtools />
    </div>
  );
}
