import React from 'react';
import * as api from '../../../api';

export type Tree = api.Tree;
export type TreeNode = api.Node;

export type TreeProps = {
  tree: Tree,
  depth: number,
  filter: (tree: Tree, depth: number) => {
    tree: boolean,
    rootLabel: boolean,
    subForest: boolean
  },
  alterTree: (tree: Tree, depth: number) => Tree,
  renderNode: RenderNode,
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
}

export type RenderNode = (tree: TreeNode, depth: number) => React.ReactNode;

const TreeView = (props: TreeProps) => {
  const tree = props.alterTree(props.tree, props.depth);
  const filter = props.filter(tree, props.depth);

  return !filter.tree ? null : (
    <div className={props.cssClasses.node} style={props.styles.node}>
      {filter.rootLabel && (
        <div className={props.cssClasses.rootLabel} style={props.styles.rootLabel}>
          {props.renderNode(tree.rootLabel, props.depth)}
        </div>
      )}
      {filter.subForest && tree.subForest.length > 0 && (
        <div className={props.cssClasses.subForest} style={props.styles.subForest}>
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
