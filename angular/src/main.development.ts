import { platformBrowserDynamic } from '@angular/platform-browser-dynamic';
import { BrowserModule } from '@angular/platform-browser';
import { NgModule } from '@angular/core';
import { GameComponent } from '@/components/game/game.component';
import { AppComponent } from './components/app/app.component';

@NgModule({
  imports: [BrowserModule, GameComponent],
  bootstrap: [AppComponent],
})
class AppModule {}

platformBrowserDynamic()
  .bootstrapModule(AppModule, { ngZone: 'noop' })
  .catch((err) => console.error(err));
