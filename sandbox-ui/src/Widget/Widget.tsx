import '../styles/fonts.css';
import '../styles/normalize.css';
import '../styles/globals.css';
import * as s from './Widget.module.css';
import * as api from '../api';

import React, { useEffect, useState } from 'react';

import Tree, { RenderNode } from '../react-haskell/Data/Tree/Tree';

export type WidgetProps = {
  serverUrl?: string,
};

export const Widget = (props: WidgetProps) => {
  const [activeTab, setActiveTab] = useState('editor');
  const [client, setClient] = useState<ReturnType<typeof api.DefaultApiFp>>();
  const [fsTree, setFsTree] = useState<api.Tree>();
  console.log('fsTree', fsTree);
  useEffect(() => {
    (async () => {
      const client = api.DefaultApiFp();
      setClient(client);
      const tree = await (await (await client.fsTreeGet(['TermFile', 'CheckFile']))()).data;
      setFsTree(tree);
    })()
  }, []);

  return (
    <div className={s.widget}>
      {fsTree && (
        <div>
          <Tree
            tree={fsTree}
            cssClasses={{ node: '', rootLabel: '', subForest: '' }}
            renderNode={renderFsTreeNode}
          />
        </div>
      )}
    </div>
  );
}

const renderFsTreeNode: RenderNode = (props) => {
  const label = props.Dir?.name || props.File?.name;
  return (<div>{label}</div>);
}
export default Widget;
