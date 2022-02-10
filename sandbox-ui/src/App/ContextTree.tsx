import React, { useEffect, useState } from 'react';
import * as s from './ContextTree.module.css';
import TreeView, { RenderNode, Tree, TreePath, pathFromUrl, pathToUrl } from '../react-haskell/Data/Tree/TreeView';
import { Link, useParams, useNavigate } from "react-router-dom";
import * as api from '../api';

type ContextTreeProps = {
  serverUrl?: string
}
type NodeCommons = {
  onClick: (path: TreePath) => void,
  isActive: (path: TreePath) => boolean
};

const ContextTree = (props: ContextTreeProps) => {
  const [_, setClient] = useState<ReturnType<typeof api.DefaultApiFp>>();
  const [tree, setTree] = useState<api.Tree>();
  const [activeNode, setActiveNode] = useState<TreePath>([]);

  const routerParams = useParams();
  const routerNavigate = useNavigate();

  useEffect(() => {
    (async () => {
      const client = api.DefaultApiFp();
      setClient(client);
      const tree = await (await (await client.fsTreeGet(['TermFile', 'CheckFile', 'ViewFile']))()).data;
      setTree(tree);

      if (!routerParams['*']) {
      const path = [getPathPart(tree)];
      const url = '/' + pathToUrl(path);
      routerNavigate(url, { replace: true });
      }
    })()
  }, []);

  useEffect(() => {
    if (routerParams['*']) {
      setActiveNode(pathFromUrl(routerParams['*']));
    }
  }, [routerParams['*']]);

  return (
    <div>
      {tree && (
        <div className={s.treeContainer}>
          <TreeView<NodeCommons>
            tree={tree}
            path={[getPathPart(tree)]}
            renderNode={renderNode}
            getPathPart={getPathPart}
            nodeCommons={{
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

const renderNode: RenderNode<NodeCommons> = (node, path, props) => {
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
      <Link className={s.fsTreeNodeName} to={'/' + pathToUrl(path)}>{label}</Link>
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

export default ContextTree;
