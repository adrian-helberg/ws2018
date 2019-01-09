package a1.donottouch;

import java.util.ArrayList;
import java.util.List;

public class Node {

	private Object content;
	private List<Node> children;

	public Node(Object content) {
		this.content = content;
		this.children = new ArrayList<>();
	}

	public void add(Node node) {
		children.add(node);
	}
	
	public List<Node> getChildren() {
		return new ArrayList<Node>(children);
	}
	
	public boolean isLeaf() {
		return children == null || children.isEmpty();
	}
	
	@Override
	public String toString() {
		return String.format("Node(%s chs:%s)", content,children);
	}

}
