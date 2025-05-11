import { createApplication } from '@angular/platform-browser';
import { provideExperimentalZonelessChangeDetection } from '@angular/core';
import { createCustomElement } from '@angular/elements';
import { GameComponent } from '@/components/game/game.component';

createApplication({
  providers: [provideExperimentalZonelessChangeDetection()],
}).then(({ injector }) => {
  customElements.define(
    'citadels-game',
    createCustomElement(GameComponent, { injector }),
  );
});
