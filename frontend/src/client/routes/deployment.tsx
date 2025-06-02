import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/deployment")({
  component: Deployment,
});

const SHA = import.meta.env.VITE_COMMIT_SHA;
const environment = SHA ? `Deployed: ${SHA}` : "Development";

function Deployment() {
  return <div>{environment}</div>;
}
