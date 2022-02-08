import '../styles/fonts.css';
import '../styles/normalize.css';
import '../styles/globals.css';
import * as s from './Widget.module.css';
import axios from 'axios';

import React, { useEffect, useState } from 'react';

import Tabs, { Tab } from './Tabs';

export type WidgetProps = {
  serverUrl?: string,
};

export const Widget = (props: WidgetProps) => {
  const [activeTab, setActiveTab] = useState('editor');
  const [fsTree, setFsTree] = useState(null);

  useEffect(() => {
  }, []);

  const tabs: Tab[] = [{
    id: 'editor',
    title: 'Editor'
  }, {
    id: 'repl',
    title: 'REPL'
  }, {
    id: 'shell',
    title: 'Shell'
  }];

  return (
    <div className={s.widget}>
      <Tabs activeTab={activeTab} tabs={tabs} onTabChange={setActiveTab} />

      <div className={s.tabs}>
        <div className={`${s.tab} ${activeTab === 'repl' ? s.activeTab : ''}`}>
          <iframe
            className={s.terminal}
            src={`http://${props.serverUrl}/repl`}
            allow='autoplay'
            frameBorder="0"
          />
        </div>
        <div className={`${s.tab} ${activeTab === 'repl' ? s.activeTab : ''}`}>
          <iframe
            className={s.terminal}
            src={`http://${props.serverUrl}/repl`}
            allow='autoplay'
            frameBorder="0"
          />
        </div>

        <div className={`${s.tab} ${activeTab === 'shell' ? s.activeTab : ''}`}>
          <iframe
            className={s.terminal}
            src={`http://${props.serverUrl}/shell`}
            allow='autoplay'
            frameBorder="0"
          />
        </div>
      </div>
    </div >
  );
}

export default Widget;
