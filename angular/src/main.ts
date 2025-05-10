import { bootstrapApplication } from '@angular/platform-browser';
import {
  DoBootstrap,
  provideExperimentalZonelessChangeDetection,
} from '@angular/core';
import { createCustomElement } from '@angular/elements';
import { BrowserModule } from '@angular/platform-browser';
import { NgModule, Injector } from '@angular/core';
import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { GameComponent } from './components/game/game.component';
import { DistrictComponent } from './components/district/district.component';

// bootstrapApplication(AppComponent, {
//   providers: [provideExperimentalZonelessChangeDetection()],
// }).catch((err) => console.error(err));

@NgModule({
  imports: [BrowserModule, GameComponent, DistrictComponent],
})
class CustomElementsModule implements DoBootstrap {
  constructor(injector: Injector) {
    customElements.define(
      'citadels-game',
      createCustomElement(GameComponent, { injector }),
    );

    customElements.define(
      'citadels-district',
      createCustomElement(DistrictComponent, { injector }),
    );
  }

  ngDoBootstrap() {} // Required to avoid auto-bootstrapping
}

platformBrowserDynamic()
  .bootstrapModule(CustomElementsModule, { ngZone: 'noop' })
  .catch((err) => console.error(err));
