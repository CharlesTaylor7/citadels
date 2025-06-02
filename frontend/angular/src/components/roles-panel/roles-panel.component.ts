import { Component, input } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RoleName } from '@/core/roles';

@Component({
  selector: 'app-roles-panel',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './roles-panel.component.html',
})
export class RolesPanelComponent {
  roles = input.required<RoleName[]>();
}
