import '../styles/fonts.css';
import '../styles/normalize.css';
import '../styles/globals.css';
import * as s from './Widget.module.css';
import * as api from '../api';

import React, { useEffect, useState } from 'react';

import TreeView, { RenderNode, Tree } from '../react-haskell/Data/Tree/TreeView';

export type WidgetProps = {
  serverUrl?: string,
};

export const Widget = (props: WidgetProps) => {
  const [client, setClient] = useState<ReturnType<typeof api.DefaultApiFp>>();
  const [fsTree, setFsTree] = useState<api.Tree>();

  useEffect(() => {
    (async () => {
      const client = api.DefaultApiFp();
      setClient(client);
      const tree = await (await (await client.fsTreeGet(['TermFile', 'CheckFile', 'ViewFile']))()).data;
      setFsTree(tree);
    })()
  }, []);

  return (
    <div className={s.widget}>
      {fsTree && (
        <div className={s.treeContainer}>
          <TreeView
            tree={fsTree}
            depth={0}
            cssClasses={{ node: s.treeNode, rootLabel: s.treeRootLabel, subForest: s.treeSubForest }}
            styles={{ node: {}, rootLabel: {}, subForest: {} }}
            renderNode={renderFsTreeNode}
            filter={(tree) => ({
              tree: !Boolean(tree.rootLabel.File?.name.match(/^default\..*$/)),
              rootLabel: tree.rootLabel.Dir?.name !== '.',
              subForest: true
            })}
            alterTree={sortFsChildren}
          />
        </div>
      )}
    </div>
  );
}

const sortFsChildren = (tree: Tree): Tree => {
  const files = tree.subForest.filter(t => t.rootLabel.File)
    .sort((a, b) => a.rootLabel.File!.name.localeCompare(b.rootLabel.File!.name));
  const dirs = tree.subForest.filter(t => t.rootLabel.Dir)
    .sort((a, b) => a.rootLabel.Dir!.name.localeCompare(b.rootLabel.Dir!.name));

  return {
    ...tree,
    subForest: [...dirs, ...files]
  }
}

const renderFsTreeNode: RenderNode = (tree, depth) => {
  let label = '';

  if (tree.Dir) {
    label = tree.Dir.name;
  } else if (tree.File && (tree.File.kind === 'TermFile' || tree.File.kind === 'CheckFile')) {
    label = tree.File?.name.replace(/\.(check|term)\.sh$/, '');
  } else if (tree.File && tree.File.kind === 'ViewFile') {
    label = tree.File.name.replace(/\.(view)\.([^.]*)$/, '.$2');
  }

  return (
    <div className={`${s.fsTreeNode} ${tree.Dir ? s.fsTreeNodeDir : s.fsTreeNodeFile}`}>
      {depth} {label}
    </div>
  );
}
export default Widget;
