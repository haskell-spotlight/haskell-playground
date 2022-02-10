import React, { useEffect, useState } from 'react';
import * as s from './Commands.module.css';
import TreeView, { RenderNode, Tree, TreePath } from '../react-haskell/Data/Tree/TreeView';
import * as api from '../api';

type CommandsProps = {
  serverUrl?: string
}
type TreeCtx = {
  onClick: (path: TreePath) => void,
  isActive: (path: TreePath) => boolean
};
const Commands = (props: CommandsProps) => {
  const [client, setClient] = useState<ReturnType<typeof api.DefaultApiFp>>();
  const [tree, setTree] = useState<api.Tree>();
  const [activeNode, setActiveNode] = useState<TreePath>([]);

  useEffect(() => {
    (async () => {
      const client = api.DefaultApiFp();
      setClient(client);
      const tree = await (await (await client.fsTreeGet(['TermFile', 'CheckFile', 'ViewFile']))()).data;
      setTree(tree);
      setActiveNode([getPathPart(tree)]);
    })()
  }, []);

  return (
    <div>
      {tree && (
        <div className={s.treeContainer}>
          <TreeView<TreeCtx>
            tree={tree}
            path={[getPathPart(tree)]}
            renderNode={renderNode}
            getPathPart={getPathPart}
            ctx={{
              onClick: (path) => setActiveNode(path),
              isActive: (path) => JSON.stringify(activeNode) === JSON.stringify(path)
            }}
          />
        </div>
      )}

    </div>
  )
}

const getPathPart = (tree: Tree) => (tree.rootLabel.Dir?.name || tree.rootLabel.File?.name) as string;
const sortSubForest = (tree: Tree): Tree => {
  const dirs = tree.subForest.filter(t => t.rootLabel.Dir)
    .sort((a, b) => a.rootLabel.Dir!.name.localeCompare(b.rootLabel.Dir!.name));

  return {
    ...tree,
    subForest: [...dirs]
  }
}

const renderNode: RenderNode<TreeCtx> = (node, path, props) => {
  const isRoot = path.length === 0;
  const isActive = props.isActive(path);
  let label = node.Dir?.name;

  const rootLabel = (
    <div
      className={`${s.fsTreeNode} ${node.Dir ? s.fsTreeNodeDir : s.fsTreeNodeFile} ${isActive ? s.fsTreeNodeActive : ''}`}
      title={label}
      onClick={() => props.onClick(path)}
    >
      {isRoot ? null : Array.from(Array(path.length - 1)).map((_, i) => <div key={i} className={s.fsTreeNodeIndent}></div>)}
      <div className={s.fsTreeNodeName}>{label}</div>
    </div>
  );

  const getVisibility = (_: Tree) => ({
    tree: true,
    rootLabel: true,
    subForest: true
  });

  const alterTree = sortSubForest;

  const cssClasses = { node: s.treeNode, rootLabel: s.treeRootLabel, subForest: s.treeSubForest };

  const styles = { node: {}, rootLabel: {}, subForest: {} };

  return {
    rootLabel,
    getVisibility,
    alterTree,
    cssClasses,
    styles
  }
}

export default Commands;
