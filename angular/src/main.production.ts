import { platformBrowser } from '@angular/platform-browser';
import { DoBootstrap } from '@angular/core';
import { createCustomElement } from '@angular/elements';
import { BrowserModule } from '@angular/platform-browser';
import { NgModule, Injector } from '@angular/core';
import { GameComponent } from '@/components/game/game.component';

@NgModule({
  imports: [BrowserModule, GameComponent],
})
class CustomElementsModule implements DoBootstrap {
  constructor(injector: Injector) {
    customElements.define(
      'citadels-game',
      createCustomElement(GameComponent, { injector }),
    );
  }
  // Required to avoid auto-bootstrapping
  ngDoBootstrap() {}
}

platformBrowser()
  .bootstrapModule(CustomElementsModule, { ngZone: 'noop' })
  .catch((err) => console.error(err));
