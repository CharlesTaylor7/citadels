import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-my-stash-panel',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './my-stash-panel.component.html',
})
export class MyStashPanelComponent {
  myPlayer = input<any>(); // Corresponds to my.* in Rust template (gold, etc.)
}
