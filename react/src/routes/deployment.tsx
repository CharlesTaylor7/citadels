import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/deployment")({
  component: Deployment,
});

// @ts-expect-error set during docker build
const SHA = import.meta.env.VITE_COMMIT_SHA;
const environment = SHA ?? "Development";

function Deployment() {
  return <div>{environment}</div>;
}
