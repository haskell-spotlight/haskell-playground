import React from 'react';
import { render } from 'react-dom';
import _Widget from './Widget/Widget';

export const Widget = _Widget;

export function renderWidget() {
  const widgetRoot = document.createElement('div');
  widgetRoot.id = 'haskell-playground-widget';

  widgetRoot.style.width = 'inherit';
  widgetRoot.style.minWidth = 'inherit';
  widgetRoot.style.height = 'inherit';
  widgetRoot.style.minHeight = 'inherit';

  document.body.appendChild(widgetRoot);

  render(<Widget sandboxUrl='localhost:8080' initialCode='supportProjectAt = "https://ko-fi.com/visortelle"'/>, widgetRoot);
}

renderWidget();
