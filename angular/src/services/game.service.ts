import { Injectable, OnDestroy } from '@angular/core';
import { Unsubscribable } from '@trpc/server/observable';
import {
  splitLink,
  createTRPCClient,
  httpBatchLink,
  httpSubscriptionLink,
  TRPCClient,
} from '@trpc/client';
import type { AppRouter } from '@citadels-types/trpc/router';

@Injectable({
  providedIn: 'root',
})
export class GameService implements OnDestroy {
  trpc: TRPCClient<AppRouter>;
  heartbeatSubscription: Unsubscribable;

  constructor() {
    const serverLink = splitLink<AppRouter>({
      condition: (op) => op.type === 'subscription',
      true: httpSubscriptionLink({ url: '/trpc' }),
      false: httpBatchLink({ url: '/trpc' }),
    });
    this.trpc = createTRPCClient({ links: [serverLink] });

    this.heartbeatSubscription = this.trpc.game.heartbeat.subscribe(undefined, {
      onData: (data) => {
        console.log('Heartbeat received:', data);
      },
      onError: (err) => {
        console.error('Heartbeat subscription error:', err);
      },
    });
  }

  ngOnDestroy(): void {
    this.heartbeatSubscription.unsubscribe();
  }
}
