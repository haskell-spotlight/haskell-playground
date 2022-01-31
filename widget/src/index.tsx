import React from 'react';
import { render } from 'react-dom';
import _Widget from './Widget/Widget';

export const Widget = _Widget;

export function renderWidget() {
  const widgetRoot = document.createElement('div');
  widgetRoot.id = 'haskell-playground-widget';
  document.body.appendChild(widgetRoot);
  render(<Widget sandboxUrl='https://abc' />, widgetRoot);
}

renderWidget();
