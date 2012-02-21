using System.Collections;
using System.Linq;

namespace Welch.Brainmess
{
    public static class Collection
    {
        public static bool IsEqualTo(this ICollection col1, ICollection col2)
        {
            if (ReferenceEquals(col1, col2)) return true;
            if (col1 == null || col2 == null) return false;

            if (col1.Count != col2.Count) return false;

            var iter1 = col1.GetEnumerator();
            var iter2 = col2.GetEnumerator();

            while(iter1.MoveNext() && iter2.MoveNext())
            {
                if (!Equals(iter1.Current, iter2.Current)) return false;
            }

            return true;
        }

        public static int HashCode(this IEnumerable items)
        {
            unchecked
            {
                if (items == null) return 0;

                return items.Cast<object>().Aggregate(17, 
                    (current, item) => current + ((19 * current) + (Equals(item, null) ? 0 : item.GetHashCode())));
            }            
        }
    }
}
