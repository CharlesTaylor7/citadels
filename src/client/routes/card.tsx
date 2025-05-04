import { useQuery } from "@tanstack/react-query";
import { queryClient, trpc } from "@/client/router";
import { District } from "@/client/components/District";
import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/card")({
  component: CardViewerComponent,
  loader: async ({ queryClient, trpc }) =>
    queryClient.ensureQueryData(trpc.asset.districts.queryOptions({})),
});

function CardViewerComponent() {
  const {
    data: cardData,
    isLoading,
    error,
  } = useQuery(trpc.asset.districts.queryOptions({}));

  return (
    <div className="card-viewer">
      <h1 className="text-2xl font-bold">Card Viewer</h1>
      {districtProps ? (
        <District {...cardData[0]} />
      ) : (
        <div>No card data available</div>
      )}
    </div>
  );
}
