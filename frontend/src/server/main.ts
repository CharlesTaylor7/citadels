import { defineEventHandler, toWebRequest } from "@tanstack/react-start/server";
import { fetchRequestHandler } from "@trpc/server/adapters/fetch";
import { appRouter as router } from "./trpc/router";
import { createContext } from "./context";

export default defineEventHandler((event) => {
  const req = toWebRequest(event);
  if (!req) {
    return new Response("No request", { status: 400 });
  }

  return fetchRequestHandler({
    endpoint: "/trpc",
    req,
    router,
    createContext,
  });
});
