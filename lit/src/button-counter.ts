import { LitElement, html } from "lit";
import { customElement, property } from "lit/decorators.js";

/**
 * An example element.
 *
 * @slot - This element has a slot
 * @csspart button - The button
 */
@customElement("button-counter")
export class ButtonCounter extends LitElement {
  /**
   * The number of times the button has been clicked.
   */
  @property({ type: Number })
  count = 0;

  // Render in light DOM instead of shadow DOM
  createRenderRoot() {
    return this;
  }
  render() {
    return html`
      <button @click="${this._onClick}" class="btn btn-primary">${this.count}</button>
    `;
  }

  private _onClick() {
    this.count++;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    "button-counter": ButtonCounter;
  }
}
