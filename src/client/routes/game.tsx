/// reference types="custom-elements"
import { createFileRoute } from "@tanstack/react-router";
import React from "react";

export const Route = createFileRoute("/game")({
  component: Game,
});

function Game() {
  return <citadels-game></citadels-game>;
}
