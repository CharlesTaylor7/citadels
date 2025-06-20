import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/v2/game")({
  component: Game,
});

function Game() {
  return (
    <>
      <citadels-district name="SecretVault"></citadels-district>
    </>
  );
}
