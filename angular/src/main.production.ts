import { createApplication } from '@angular/platform-browser';
import { provideExperimentalZonelessChangeDetection } from '@angular/core';
import { createCustomElement } from '@angular/elements';
import { DistrictComponent } from './components/district/district.component';

createApplication({
  providers: [provideExperimentalZonelessChangeDetection()],
}).then(({ injector }) => {
  customElements.define(
    'citadels-district',
    createCustomElement(DistrictComponent, { injector }),
  );
});
