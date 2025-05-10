import { Component } from '@angular/core';
import { GameComponent } from '@/components/game/game.component';

@Component({
  selector: 'app-root',
  standalone: true,
  imports: [GameComponent],
  templateUrl: './app.component.html',
})
export class AppComponent {
  title = 'citadels';
}
