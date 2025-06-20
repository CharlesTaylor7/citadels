import { Link, createFileRoute } from "@tanstack/react-router";
import { useNavigate } from "@tanstack/react-router";
import { useForm } from "react-hook-form";
import { useQuery, useMutation } from "@/api";
import { components } from "@/schema";

export const Route = createFileRoute("/signup")({
  component: SignupComponent,
});

type FormFields = components["schemas"]["UserSignup"];

function SignupComponent() {
  const navigate = useNavigate();

  const form = useForm<FormFields>();
  const username = form.watch("username");
  const usernameQuery = useQuery(
    "get",
    "/auth/check-username",
    {
      params: { query: { username } },
    },
    { enabled: !!username },
  );
  const signupMutation = useMutation("post", "/auth/signup", {
    onSuccess: () => {
      navigate("/");
    },
  });

  return (
    <div className="hero min-h-screen bg-base-200">
      <div className="hero-content flex-col">
        <div className="text-center">
          <h1 className="text-5xl font-bold">Sign Up</h1>
          <p className="py-6">Create your account</p>
        </div>
        <div className="card flex-shrink-0 w-full max-w-sm shadow-2xl bg-base-100">
          <form
            className="card-body"
            onSubmit={form.handleSubmit((body) =>
              signupMutation.mutate({ body }),
            )}
          >
            <div className="form-control">
              <label className="label">
                <span className="label-text">Username</span>
              </label>
              <input
                {...form.register("username")}
                type="username"
                placeholder="username"
                className="input input-bordered"
                required
                disabled={signupMutation.isPending}
              />
              {usernameQuery.data?.taken && (
                <span className="text-error">Username taken</span>
              )}
            </div>
            <div className="form-control">
              <label className="label">
                <span className="label-text">Email</span>
              </label>
              <input
                {...form.register("email")}
                type="email"
                placeholder="email"
                className="input input-bordered"
                required
                disabled={signupMutation.isPending}
              />
            </div>
            <div className="form-control">
              <label className="label">
                <span className="label-text">Password</span>
              </label>
              <input
                {...form.register("password")}
                type="password"
                className="input input-bordered"
                required
                disabled={signupMutation.isPending}
              />
            </div>
            <div className="form-control mt-6">
              <button
                type="submit"
                className="btn btn-primary"
                disabled={signupMutation.isPending}
              >
                {signupMutation.isPending ? (
                  <span className="loading loading-spinner loading-sm"></span>
                ) : (
                  "Sign Up"
                )}
              </button>
            </div>
            <div className="text-center mt-4">
              <p>
                Already have an account?{" "}
                <Link to="/login" className="link link-primary">
                  Login
                </Link>
              </p>
            </div>
          </form>
        </div>
      </div>
    </div>
  );
}
