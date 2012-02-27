package welch.brainmess;

import java.util.LinkedList;
import java.util.List;

public class ListFormatter<T> {
	private final ElementFormatter<T> _elementFormatter;
	private final String _prefix;
	private final String _suffix;
	private final String _separator;
	
	public ListFormatter(String prefix, String suffix, String separator, 
			ElementFormatter<T> elementFormatter) {
		if (prefix == null) throw new NullPointerException("prefix");
		if (suffix == null) throw new NullPointerException("suffix");
		if (separator == null) throw new NullPointerException("separator");
		if (elementFormatter == null) throw new NullPointerException("elementFormatter");
		
		_prefix = prefix;
		_suffix = suffix;
		_separator = separator;
		_elementFormatter = elementFormatter;
	}

	
	public String format(List<T> list) {
		StringBuilder builder = new StringBuilder(_prefix);
		boolean first = true;
		int index = 0;
		for(T t : list) {
			if (first) {
				first = false;
			} else {
				builder.append(_separator);
			}
			builder.append(_elementFormatter.format(t, index));
			index++;
		}
		builder.append(_suffix);
		return builder.toString();
	}
	
	public interface ElementFormatter<E> {
		String format(E t, int index);
	}
	
	public class DefaultElementFormatter<E> implements ElementFormatter<E> {
		@Override
		public String format(E t, int index) {
			return t.toString();
		}
	}
	
	public static void main(String[] args) {
		new LinkedList<Integer>(null);
	}
}
