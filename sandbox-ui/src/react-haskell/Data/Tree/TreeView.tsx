import React from 'react';
import * as api from '../../../api';

export type Tree = api.Tree;
export type TreeNode = api.Node;

export type TreeProps = {
  tree: Tree,
  depth: number,
  renderNode: RenderNode,
}

export type RenderNode = (tree: TreeNode, depth: number) => ({
  getVisibility: (tree: Tree, depth: number) => {
    tree: boolean,
    rootLabel: boolean,
    subForest: boolean
  },
  alterTree: (tree: Tree, depth: number) => Tree,
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

const TreeView = (props: TreeProps) => {
  const { alterTree, getVisibility, rootLabel, cssClasses, styles } = props.renderNode(props.tree.rootLabel, props.depth);
  const tree = alterTree(props.tree, props.depth);
  const visibility = getVisibility(tree, props.depth);

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
              depth={props.depth + 1}
            />
          ))}
        </div>
      )}
    </div>
  );
}

export default TreeView;
