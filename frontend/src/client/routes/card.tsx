import { useQuery } from "@tanstack/react-query";
import { queryClient, trpc } from "@/client/router";
import { District } from "@/client/components/District";
import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/card")({
  component: CardViewerComponent,
});

function CardViewerComponent() {
  return (
    <div className="card-viewer">
      <h1 className="text-2xl font-bold">Card Viewer</h1>
      <div className="flex gap-2">
        <District name="Temple" enabled square height={500} />
        <District name="Temple" enabled height={500} />
      </div>
    </div>
  );
}
