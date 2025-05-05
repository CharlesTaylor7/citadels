import { Link, createFileRoute } from "@tanstack/react-router";
import { trpc } from "@/client/router";
import { useMutation, useQuery, useQueryClient } from "@tanstack/react-query";
import { useSubscription } from "@trpc/tanstack-react-query";

export const Route = createFileRoute("/lobby")({
  component: LobbyComponent,
  loader: async ({ context: { trpc, queryClient } }) => {
    await queryClient.ensureQueryData(trpc.lobby.rooms.queryOptions());
  },
});

function LobbyComponent() {
  const queryClient = useQueryClient();
  useSubscription({
    ...trpc.lobby.subscribe.subscriptionOptions(),
    onData: () =>
      queryClient.invalidateQueries({
        queryKey: trpc.lobby.rooms.queryKey(),
      }),
  });

  const userQuery = useQuery(trpc.auth.me.queryOptions());
  const roomsQuery = useQuery(trpc.lobby.rooms.queryOptions());

  const userId = userQuery.data?.userId;
  const rooms = roomsQuery.data || [];
  const userRoom = rooms.find((r) => r.members.some((m) => m.id === userId));

  const startGameMutation = useMutation({
    ...trpc.lobby.startGame.mutationOptions(),
  });
  const leaveRoomMutation = useMutation({
    ...trpc.lobby.leaveRoom.mutationOptions(),
  });
  const createRoomMutation = useMutation({
    ...trpc.lobby.createRoom.mutationOptions(),
  });
  const joinRoomMutation = useMutation({
    ...trpc.lobby.joinRoom.mutationOptions(),
  });
  const transferOwnershipMutation = useMutation({
    ...trpc.lobby.transferOwnership.mutationOptions(),
  });
  const claimOwnershipMutation = useMutation({
    ...trpc.lobby.claimOwnership.mutationOptions(),
  });

  return (
    <>
      <div className="bg-base-100 rounded-box shadow-sm mb-6">
        <h1 className="text-2xl font-bold">Game Lobby</h1>
      </div>

      <div className="mb-6">
        <button
          onClick={() => createRoomMutation.mutate()}
          disabled={
            createRoomMutation.isPending || userRoom != null || userId == null
          }
          className="btn btn-primary"
        >
          Create Room
        </button>
        {createRoomMutation.isPending ? "creating..." : ""}
      </div>

      <h2 className="text-xl font-semibold mb-4">Available Rooms</h2>

      {roomsQuery.isPending && rooms.length === 0 ? (
        <div className="alert alert-info">
          <div>
            <svg
              xmlns="http://www.w3.org/2000/svg"
              fill="none"
              viewBox="0 0 24 24"
              className="stroke-current shrink-0 w-6 h-6"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth="2"
                d="M13 16h-1v-4h-1m1-4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z"
              ></path>
            </svg>
            <span>No rooms available. Create one to get started!</span>
          </div>
        </div>
      ) : (
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {rooms.map((room) => {
            return (
              <div
                key={room.id}
                className={`card ${room.members.some((m) => m.id === userId) ? "bg-base-200" : "bg-base-100"} shadow-md relative`}
              >
                <div className="card-body">
                  <div className="absolute top-2 right-2 flex gap-2"></div>

                  <div className="mb-2">
                    <div>
                      <div className="font-medium mb-1">
                        {room.members.length} players
                      </div>
                      <div className="max-h-32 overflow-y-auto bg-base-100 rounded-box p-2 border border-base-300">
                        {room.members.map((member, index) => (
                          <div key={index} className="flex items-center py-1">
                            <span className="badge badge-sm badge-success mr-2"></span>
                            <span>{member.name}</span>
                            {member.owner && (
                              <div className="badge badge-warning badge-sm ml-2">
                                Owner
                              </div>
                            )}
                            {!member.owner &&
                              room.members.find((m) => m.id === userId)
                                ?.owner && (
                                <button
                                  onClick={() =>
                                    transferOwnershipMutation.mutate({
                                      roomId: room.id,
                                      newOwnerId: member.id,
                                    })
                                  }
                                  disabled={transferOwnershipMutation.isPending}
                                  className="btn btn-secondary btn-xs ml-2"
                                >
                                  Transfer
                                </button>
                              )}
                          </div>
                        ))}
                      </div>
                    </div>
                  </div>

                  <div className="flex gap-2">
                    {room.gameId ? (
                      <Link to="/game" className="btn btn-primary btn-sm">
                        Continue Playing
                      </Link>
                    ) : userRoom?.id === room.id ? (
                      <button
                        onClick={() =>
                          leaveRoomMutation.mutate({ roomId: room.id })
                        }
                        disabled={leaveRoomMutation.isPending}
                        className="btn btn-warning btn-sm"
                      >
                        Leave
                      </button>
                    ) : (
                      <button
                        onClick={() =>
                          joinRoomMutation.mutate({ roomId: room.id })
                        }
                        disabled={joinRoomMutation.isPending}
                        className="btn btn-primary "
                      >
                        Join
                      </button>
                    )}

                    {!room.owner && (
                      <button
                        onClick={() =>
                          claimOwnershipMutation.mutate({ roomId: room.id })
                        }
                        disabled={claimOwnershipMutation.isPending}
                        className="btn btn-info btn-sm"
                      >
                        Claim Ownership
                      </button>
                    )}

                    {room.owner?.id === userId ? (
                      <button
                        onClick={() => startGameMutation.mutate()}
                        disabled={
                          startGameMutation.isPending || room.members.length < 2
                        }
                        className="btn btn-success btn-sm"
                      >
                        Start Game
                      </button>
                    ) : null}
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      )}
    </>
  );
}
