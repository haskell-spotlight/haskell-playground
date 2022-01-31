import '../styles/fonts.css';
import '../styles/normalize.css';
import '../styles/globals.css';
import * as s from './Widget.module.css';

import React, { useRef, useEffect, RefObject, useState } from 'react';
import Tabs, { Tab } from './Tabs';

export type WidgetProps = {
  sandboxUrl: string,
};

export const Widget = (props: WidgetProps) => {
  const [activeTab, setActiveTab] = useState('repl');

  const tabs: Tab[] = [{
    id: 'shell',
    title: 'Shell'
  }, {
    id: 'repl',
    title: 'REPL'
  }];

  return (
    <div className={s.widget}>
      <Tabs activeTab={activeTab} tabs={tabs} onTabChange={setActiveTab} />

      <iframe
        className={`${s.tab} ${activeTab === 'repl' ? s.activeTab : ''}`}
        src={`http://${props.sandboxUrl}/repl`}
        allow='autoplay'
        frameBorder="0"
      />

      <iframe
        className={`${s.tab} ${activeTab === 'shell' ? s.activeTab : ''}`}
        src={`http://${props.sandboxUrl}/shell`}
        allow='autoplay'
        frameBorder="0"
      />
    </div>
  );
}

export default Widget;
