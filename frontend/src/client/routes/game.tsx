import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/game")({
  component: Game,
});

function Game() {
  // @ts-expect-error custom element
  return <citadels-game></citadels-game>;
}
