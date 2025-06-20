import { Link, useNavigate } from "@tanstack/react-router";
import { useQuery, useMutation } from "@/api";

export function Navbar() {
  const navigate = useNavigate();
  const userQuery = useQuery("get", "/auth/me", {});
  const logoutMutation = useMutation("post", "/auth/logout", {
    onSuccess: () => navigate({ to: "/" }),
  });
  return (
    <nav className="navbar bg-base-200 shadow-lg">
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
    </nav>
  );
}
