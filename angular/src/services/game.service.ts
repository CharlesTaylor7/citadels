import { Injectable } from '@angular/core';
import {
  splitLink,
  createTRPCClient,
  httpBatchLink,
  httpSubscriptionLink,
  TRPCClient,
} from '@trpc/client';
import type { AppRouter } from "@citadels-server/trpc/router";

@Injectable({
  providedIn: 'root',
})
export class GameService
{
  trpc: TRPCClient<AppRouter>;
  constructor() {
    const serverLink = splitLink<AppRouter>({
      condition: (op) => op.type === 'subscription',
      true: httpSubscriptionLink({ url: '/trpc' }),
      false: httpBatchLink({ url: '/trpc' }),
    });
    this.trpc = createTRPCClient({ links: [serverLink] });
  }
  ngOnInit() {

    this.trpc.game.subscribe({ id: 1 }, {
      next: (data) => console.log(data),
  }
}
