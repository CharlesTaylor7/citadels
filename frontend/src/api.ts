import createFetchClient from "openapi-fetch";
import createClient from "openapi-react-query";
import type { paths } from "@/schema";

const fetchClient = createFetchClient<paths>({
  baseUrl: "https://myapi.dev/v1/",
});
export const api = createClient(fetchClient);
export const { useQuery, useMutation, useInfiniteQuery, useSuspenseQuery } =
  api;
