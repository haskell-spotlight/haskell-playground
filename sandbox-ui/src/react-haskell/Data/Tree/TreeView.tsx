import React from 'react';
import * as api from '../../../api';

export type Tree = api.Tree;
export type TreeNode = api.Node;
export type TreePath = string[];

export type TreeProps<CTX> = {
  tree: Tree,
  path: TreePath,
  renderNode: RenderNode<CTX>,
  getPathPart: (tree: Tree) => string,
  ctx: CTX
}

export type RenderNode<CTX> = (node: TreeNode, path: TreePath, ctx: CTX) => ({
  getVisibility: (tree: Tree, path: TreePath) => {
    tree: boolean,
    rootLabel: boolean,
    subForest: boolean
  },
  alterTree: (tree: Tree, path: TreePath) => Tree,
  rootLabel: React.ReactNode,
  cssClasses: {
    node: string,
    rootLabel: string,
    subForest: string
  },
  styles: {
    node: React.CSSProperties,
    rootLabel: React.CSSProperties,
    subForest: React.CSSProperties
  }
});

function TreeView<CTX>(props: TreeProps<CTX>) {
  const pathPart = props.getPathPart(props.tree);
  const path = props.path.concat(pathPart);
  const { alterTree, getVisibility, rootLabel, cssClasses, styles } = props.renderNode(props.tree.rootLabel, path, props.ctx);
  const tree = alterTree(props.tree, props.path);
  const visibility = getVisibility(tree, props.path);

  return !visibility.tree ? null : (
    <div className={cssClasses.node} style={styles.node}>
      {visibility.rootLabel && (
        <div className={cssClasses.rootLabel} style={styles.rootLabel}>
          {rootLabel}
        </div>
      )}
      {visibility.subForest && tree.subForest.length > 0 && (
        <div className={cssClasses.subForest} style={styles.subForest}>
          {tree.subForest.map(tree => (
            <TreeView
              key={JSON.stringify(tree.rootLabel)}
              {...props}
              tree={tree}
              path={path}
            />
          ))}
        </div>
      )}
    </div>
  );
}

export default TreeView;
