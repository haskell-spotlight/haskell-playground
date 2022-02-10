import React from 'react';
import * as api from '../../../api';

export type Tree = api.Tree;
export type TreeNode = api.Node;
export type TreePath = string[];

export const pathToUrl = (path: TreePath): string => path.map(encodeURIComponent).join('/');
export const pathFromUrl = (url: string): TreePath => url.split('/').map(decodeURIComponent);

export type TreeProps<NC> = {
  tree: Tree,
  path: TreePath,
  renderNode: RenderNode<NC>,
  getPathPart: (tree: Tree) => string,
  nodeCommons: NC
}

export type RenderNode<NC> = (node: TreeNode, path: TreePath, ctx: NC) => ({
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

function TreeView<NC>(props: TreeProps<NC>) {
  const { alterTree, getVisibility, rootLabel, cssClasses, styles } = props.renderNode(props.tree.rootLabel, props.path, props.nodeCommons);
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
          {tree.subForest.map(tree => {
            const pathPart = props.getPathPart(tree);
            const path = props.path.concat([pathPart]);
            return (
              <TreeView
                key={path.reduce((k, p) => k + p, '')}
                {...props}
                tree={tree}
                path={path}
              />
            )
          })}
        </div>
      )}
    </div>
  );
}

export default TreeView;
