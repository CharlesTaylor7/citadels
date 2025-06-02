import * as React from "react";
import { Link, createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/")({
  component: About,
});

function About() {
  return <>Play Citadels</>;
}
