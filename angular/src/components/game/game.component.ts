import { GameService } from '@/services/game.service';
import { Component } from '@angular/core';

@Component({
  selector: 'app-game',
  standalone: true,
  templateUrl: './game.component.html',
})
export class GameComponent {
  constructor(private gameService: GameService) {}
}
