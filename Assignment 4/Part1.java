import java.util.*;

public class Part1 {
	
	public static class SortedList<T extends Comparable<T>> extends LinkedList<T> implements Comparable<SortedList<T>> {

		private static final long serialVersionUID = 1L;
		
		public SortedList() {
			super();
		}
		
		public SortedList(Collection<? extends T> c) {
			Iterator<? extends T> i = c.iterator();
			while (i.hasNext()) {
				this.add(i.next());
			}
		}
		
		@Override
		public boolean add(T a) {
			if (this.isEmpty()) {
				super.add(a);
				return true;
			}
			if (a.compareTo(this.getLast()) > 0) {
				super.addLast(a);
				return true;
			}
			if (a.compareTo(this.getFirst()) < 0) {
				super.addFirst(a);
				return true;
			}
			int index = Collections.binarySearch(this, a);
			if (index < 0) index = -(index + 1);
			List<T> temp = new LinkedList<T>(this);
			List<T> left = temp.subList(0, index);
			List<T> right = temp.subList(index, temp.size());
			this.clear();
			this.addAll(left);
			this.add(a);
			this.addAll(right);
			return true;
		}

		@Override
		public int compareTo(SortedList<T> other) {
			int min = Math.min(this.size(), other.size());
			for (int i = 0; i < min; i++) {
				if (this.get(i).compareTo(other.get(i)) < 0)
					return -1;
				else if (this.get(i).compareTo(other.get(i)) > 0)
					return 1;
			}
			return Integer.signum(this.size() - other.size());
		}
		
		@Override
		public String toString() {
			return "[" + Arrays.deepToString(this.toArray()).replaceAll(", ", " ") + "]";
		}
		
	}
	
	public static class A implements Comparable<A> {
		
		private int x;
		
		public A(int x) {
			this.setX(x);
		}
		
		public int getX() {
			return this.x;
		}

		public void setX(int x) {
			this.x = x;
		}

		@Override
		public int compareTo(A other) {
			return Integer.signum(this.getX() - other.getX());
		}
		
		@Override
		public String toString() {
			return "A<" + this.getX() + ">";
		}
		
	}
	
	public static class B extends A {
		
		private int y, z;

		public B(int y, int z) {
			super(y + z);
			this.setY(y);
			this.setZ(z);
		}
		
		public int getY() {
			return this.y;
		}

		public void setY(int y) {
			this.y = y;
		}
		
		public int getZ() {
			return this.z;
		}

		public void setZ(int z) {
			this.z = z;
		}
		
		@Override
		public String toString() {
			return "B<" + this.getY() + "," + this.getZ() + ">";
		}
		
	}
	
	private static <T> boolean addToSortedList(SortedList<? super T> L, T z) {
		return L.add(z);
	}
	
	private static void test() {
		SortedList<A> c1 = new SortedList<A>();
		SortedList<A> c2 = new SortedList<A>();
		for (int i = 35; i >= 0; i -= 5) {
			addToSortedList(c1, new A(i));
			addToSortedList(c2, new B(i + 2, i + 3));
		}

		System.out.print("c1: ");
		System.out.println(c1);

		System.out.print("c2: ");
		System.out.println(c2);

		switch (c1.compareTo(c2)) {
		case -1:
			System.out.println("c1 < c2");
			break;
		case 0:
			System.out.println("c1 = c2");
			break;
		case 1:
			System.out.println("c1 > c2");
			break;
		default:
			System.out.println("Uh Oh");
			break;
		}

	}
	
	public static void main(String[] args) {
		test();
	}

}
