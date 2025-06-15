import createFetchClient from "openapi-fetch";
import createReactQueryClient from "openapi-react-query";
import type { paths } from "@/schema";

const client = createFetchClient<paths>({
  baseUrl: "/api",
});

client.use({
  onResponse({ request, response, options }) {
    if (response.status >= 400) {
      const err = new Error("");
      //
      err.cause = response;
      throw err;
    }
  },
});
export const api = createReactQueryClient(client);
export const { useQuery, useMutation, useInfiniteQuery, useSuspenseQuery } =
  api;
