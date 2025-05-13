import {
  Injectable,
  OnDestroy,
  signal,
  effect,
  DestroyRef,
  inject,
} from '@angular/core';
import {
  splitLink,
  createTRPCClient,
  httpBatchLink,
  httpSubscriptionLink,
  TRPCClient,
} from '@trpc/client';
import type { AppRouter } from '@/types/trpc/router';
import { DistrictName } from '@/core/districts';
import { RoleName } from '@/core/roles';

@Injectable({
  providedIn: 'root',
})
export class GameService {
  trpc: TRPCClient<AppRouter>;
  gameStateSignal = signal<FrontendState | null>(null);

  constructor() {
    const serverLink = splitLink<AppRouter>({
      condition: (op) => op.type === 'subscription',
      true: httpSubscriptionLink({ url: '/trpc' }),
      false: httpBatchLink({ url: '/trpc' }),
    });
    this.trpc = createTRPCClient({ links: [serverLink] });

    const subscription = this.trpc.game.heartbeat.subscribe(undefined, {
      onData: (data) => {
        console.log('Heartbeat received:', data);
      },
      onError: (err) => {
        console.error('Heartbeat subscription error:', err);
      },
    });
    const ref = inject(DestroyRef);
    ref.onDestroy(subscription.unsubscribe);
  }
}

interface FrontendState {
  myHand: Hand;
}
interface Hand {
  districts: DistrictName[];
  roles: RoleName[];
}
