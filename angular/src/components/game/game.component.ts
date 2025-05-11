import { GameService } from '@/services/game.service';
import { Component } from '@angular/core';
import { DistrictComponent } from '../district/district.component';

@Component({
  selector: 'app-game',
  standalone: true,
  templateUrl: './game.component.html',
  imports: [DistrictComponent],
})
export class GameComponent {
  constructor(private gameService: GameService) {}
}
