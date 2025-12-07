/**
 * @fileoverview Sorting algorithm implementations with step logging.
 * 
 * All sorting algorithms work on pure number arrays (indices), completely
 * independent of image data. Each algorithm logs every comparison and swap
 * operation as a step for visualization purposes.
 * 
 * All algorithms must work in-place and only use the defined swap
 * and comparison operators for working with array elements. No temporary arrays or inserts.
 */

/**
 * Base comparison function for sorting algorithms.
 * Compares two elements in an array based on their numeric values.
 * This ensures consistent ordering across all sorting algorithms.
 * Automatically logs the comparison step.
 * 
 * @param {number[]} arr - The array being sorted
 * @param {number} i - First index to compare
 * @param {number} j - Second index to compare
 * @param {import('./types.js').SortingStep[]} steps - Steps array to log to
 * @returns {number} Negative if arr[i] < arr[j], positive if arr[i] > arr[j], 0 if equal
 */
function compare(arr, i, j, steps) {
    steps.push({ type: 'compare', indices: [i, j] });
    return arr[i] - arr[j];
}

/**
 * Swaps two elements in an array (in-place).
 * Automatically logs the swap step.
 * 
 * @param {number[]} arr - The array to modify
 * @param {number} i - First index
 * @param {number} j - Second index
 * @param {import('./types.js').SortingStep[]} steps - Steps array to log to
 */
function swap(arr, i, j, steps) {
    const temp = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
    steps.push({ type: 'swap', indices: [i, j] });
}

export { compare, swap };

/**
 * Bubble Sort algorithm with step logging.
 * Repeatedly steps through the array, compares adjacent elements and swaps them
 * if they are in the wrong order. The pass through the array is repeated until
 * the array is sorted.
 * 
 * Time Complexity: O(n²)
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function bubbleSort(arr) {
    const steps = [];
    const n = arr.length;
    
    for (let i = 0; i < n - 1; i++) {
        let swapped = false;
        
        for (let j = 0; j < n - i - 1; j++) {
            // Compare adjacent elements
            if (compare(arr, j, j + 1, steps) > 0) {
                // Swap if they're in wrong order
                swap(arr, j, j + 1, steps);
                swapped = true;
            }
        }
        
        // If no swaps occurred, array is sorted
        if (!swapped) {
            break;
        }
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Bubble Sort algorithm object for export.
 */
const BubbleSort = {
    name: 'Bubble Sort',
    description: 'Repeatedly compares and swaps adjacent elements. Simple but inefficient for large datasets.',
    characteristics: {
        timeComplexity: 'O(n²)',
        spaceComplexity: 'O(1)',
        stable: true,
        bestFor: 'Small datasets or educational purposes'
    },
    sort: bubbleSort
};

export { BubbleSort };

/**
 * Selection Sort algorithm with step logging.
 * Divides the array into sorted and unsorted regions. Repeatedly finds the
 * minimum element from the unsorted region and moves it to the end of the
 * sorted region.
 * 
 * Time Complexity: O(n²)
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function selectionSort(arr) {
    const steps = [];
    const n = arr.length;
    
    for (let i = 0; i < n - 1; i++) {
        let minIndex = i;
        
        // Find the minimum element in the unsorted portion
        for (let j = i + 1; j < n; j++) {
            if (compare(arr, j, minIndex, steps) < 0) {
                minIndex = j;
            }
        }
        
        // Swap the found minimum element with the first element of unsorted portion
        if (minIndex !== i) {
            swap(arr, i, minIndex, steps);
        }
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Selection Sort algorithm object for export.
 */
const SelectionSort = {
    name: 'Selection Sort',
    description: 'Finds the minimum element and places it at the beginning. Makes fewer swaps than bubble sort.',
    characteristics: {
        timeComplexity: 'O(n²)',
        spaceComplexity: 'O(1)',
        stable: false,
        bestFor: 'Small datasets where swap cost is high'
    },
    sort: selectionSort
};

export { SelectionSort };

/**
 * Insertion Sort algorithm with step logging.
 * Builds the sorted array one element at a time by repeatedly taking the next
 * element and inserting it into its correct position in the sorted portion.
 * 
 * Time Complexity: O(n²) worst case, O(n) best case (already sorted)
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function insertionSort(arr) {
    const steps = [];
    const n = arr.length;
    
    for (let i = 1; i < n; i++) {
        let j = i;
        
        // Move the element at position i to its correct position in the sorted portion
        while (j > 0) {
            if (compare(arr, j - 1, j, steps) > 0) {
                // Swap if previous element is larger
                swap(arr, j - 1, j, steps);
                j--;
            } else {
                // Found correct position, break
                break;
            }
        }
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Insertion Sort algorithm object for export.
 */
const InsertionSort = {
    name: 'Insertion Sort',
    description: 'Builds sorted array one element at a time by inserting each element into its correct position. Efficient for small or nearly sorted data.',
    characteristics: {
        timeComplexity: 'O(n²) worst, O(n) best',
        spaceComplexity: 'O(1)',
        stable: true,
        bestFor: 'Small or nearly sorted datasets'
    },
    sort: insertionSort
};

export { InsertionSort };

/**
 * Quick Sort algorithm with step logging.
 * Uses divide-and-conquer approach by selecting a pivot element and partitioning
 * the array around it, then recursively sorting the sub-arrays.
 * 
 * Time Complexity: O(n log n) average, O(n²) worst case
 * Space Complexity: O(log n) for recursion stack
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function quickSort(arr) {
    const steps = [];
    
    /**
     * Partition helper function.
     * Chooses the last element as pivot and partitions the array.
     * 
     * @param {number} low - Starting index
     * @param {number} high - Ending index
     * @returns {number} Final position of the pivot
     */
    function partition(low, high) {
        let i = low - 1;
        
        for (let j = low; j < high; j++) {
            if (compare(arr, j, high, steps) <= 0) {
                i++;
                if (i !== j) {
                    swap(arr, i, j, steps);
                }
            }
        }
        
        // Place pivot in its final position
        if (i + 1 !== high) {
            swap(arr, i + 1, high, steps);
        }
        
        return i + 1;
    }
    
    /**
     * Recursive quick sort helper.
     * 
     * @param {number} low - Starting index
     * @param {number} high - Ending index
     */
    function quickSortRecursive(low, high) {
        if (low < high) {
            const pivotIndex = partition(low, high);
            quickSortRecursive(low, pivotIndex - 1);
            quickSortRecursive(pivotIndex + 1, high);
        }
    }
    
    if (arr.length > 0) {
        quickSortRecursive(0, arr.length - 1);
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Quick Sort algorithm object for export.
 */
const QuickSort = {
    name: 'Quick Sort',
    description: 'Divide-and-conquer algorithm that partitions array around a pivot. Very efficient for large datasets.',
    characteristics: {
        timeComplexity: 'O(n log n) avg, O(n²) worst',
        spaceComplexity: 'O(log n)',
        stable: false,
        bestFor: 'Large datasets with random data'
    },
    sort: quickSort
};

export { QuickSort };

/**
 * Merge Sort algorithm with in-place merge and step logging.
 * Uses divide-and-conquer by recursively dividing the array in half,
 * sorting each half, and merging them back together. The merge is done
 * in-place using rotation/shifting to generate swap operations for visualization.
 * 
 * Time Complexity: O(n log n)
 * Space Complexity: O(log n) for recursion stack (in-place merge)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function mergeSort(arr) {
    const steps = [];
    
    /**
     * In-place merge using rotation.
     * Merges two sorted subarrays [left, mid] and [mid+1, right] without auxiliary array.
     * 
     * @param {number} left - Start index of first subarray
     * @param {number} mid - End index of first subarray
     * @param {number} right - End index of second subarray
     */
    function merge(left, mid, right) {
        let start = left;
        let start2 = mid + 1;
        
        // Merge the two sorted subarrays
        while (start <= mid && start2 <= right) {
            // If element in first subarray is in correct position
            if (compare(arr, start, start2, steps) <= 0) {
                start++;
            } else {
                // Element at start2 needs to be moved to position start
                // Rotate it into position using adjacent swaps
                let index = start2;
                while (index > start) {
                    swap(arr, index, index - 1, steps);
                    index--;
                }
                
                // Update pointers - one element from right subarray has been merged
                start++;
                mid++;
                start2++;
            }
        }
    }
    
    /**
     * Recursive merge sort helper.
     * 
     * @param {number} left - Starting index
     * @param {number} right - Ending index
     */
    function mergeSortRecursive(left, right) {
        if (left < right) {
            const mid = Math.floor((left + right) / 2);
            
            // Sort first half
            mergeSortRecursive(left, mid);
            
            // Sort second half
            mergeSortRecursive(mid + 1, right);
            
            // Merge the sorted halves
            merge(left, mid, right);
        }
    }
    
    if (arr.length > 0) {
        mergeSortRecursive(0, arr.length - 1);
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Merge Sort algorithm object for export.
 */
const MergeSort = {
    name: 'Merge Sort',
    description: 'Divide-and-conquer algorithm that recursively divides array in half and merges sorted halves. Stable and predictable.',
    characteristics: {
        timeComplexity: 'O(n log n)',
        spaceComplexity: 'O(log n) in-place',
        stable: true,
        bestFor: 'Large datasets requiring stable sort'
    },
    sort: mergeSort
};

export { MergeSort };

/**
 * Radix Sort (LSD) algorithm with step logging.
 * Sorts integers by processing digits from least significant to most significant.
 * Uses counting sort as a subroutine for each digit position.
 * 
 * Time Complexity: O(d * n) where d is the number of digits
 * Space Complexity: O(n) for auxiliary arrays
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function radixSort(arr) {
    const steps = [];
    
    if (arr.length <= 1) {
        steps.push({ type: 'complete' });
        return steps;
    }
    
    // Find the maximum value to determine number of digits
    let max = arr[0];
    for (let i = 1; i < arr.length; i++) {
        compare(arr, i, 0, steps); // Log comparison for visualization
        if (arr[i] > max) {
            max = arr[i];
        }
    }
    
    // Process each digit position (LSD to MSD)
    for (let exp = 1; Math.floor(max / exp) > 0; exp *= 10) {
        countingSortByDigit(arr, exp, steps);
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Counting sort subroutine for radix sort.
 * Sorts array based on digit at given exponent position.
 * 
 * @param {number[]} arr - Array to sort
 * @param {number} exp - Current digit position (1, 10, 100, etc.)
 * @param {import('./types.js').SortingStep[]} steps - Steps array to log to
 */
function countingSortByDigit(arr, exp, steps) {
    const n = arr.length;
    const output = new Array(n);
    const count = new Array(10).fill(0);
    
    // Store count of occurrences of each digit
    for (let i = 0; i < n; i++) {
        const digit = Math.floor(arr[i] / exp) % 10;
        count[digit]++;
    }
    
    // Change count[i] to contain actual position of this digit in output
    for (let i = 1; i < 10; i++) {
        count[i] += count[i - 1];
    }
    
    // Build the output array by placing elements in sorted order
    for (let i = n - 1; i >= 0; i--) {
        const digit = Math.floor(arr[i] / exp) % 10;
        output[count[digit] - 1] = arr[i];
        count[digit]--;
    }
    
    // Copy the output array back to arr with swaps for visualization
    for (let i = 0; i < n; i++) {
        if (arr[i] !== output[i]) {
            // Find where the correct value is currently located
            let sourceIndex = i;
            for (let j = i + 1; j < n; j++) {
                if (arr[j] === output[i]) {
                    sourceIndex = j;
                    break;
                }
            }
            
            // Swap to move it into position
            if (sourceIndex !== i) {
                compare(arr, i, sourceIndex, steps);
                swap(arr, i, sourceIndex, steps);
            }
        }
    }
}

/**
 * Radix Sort algorithm object for export.
 */
const RadixSort = {
    name: 'Radix Sort',
    description: 'Non-comparison sort that processes digits from least to most significant. Very efficient for integers.',
    characteristics: {
        timeComplexity: 'O(d·n) where d is digits',
        spaceComplexity: 'O(n)',
        stable: true,
        bestFor: 'Integer data with limited range'
    },
    sort: radixSort
};

export { RadixSort };

/**
 * Heap Sort algorithm with step logging.
 * Builds a max heap and repeatedly extracts the maximum element.
 * 
 * Time Complexity: O(n log n)
 * Space Complexity: O(log n) for recursion
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function heapSort(arr) {
    const steps = [];
    const n = arr.length;
    
    /**
     * Heapify a subtree rooted at index i.
     * 
     * @param {number} n - Size of heap
     * @param {number} i - Root index
     */
    function heapify(n, i) {
        let largest = i;
        const left = 2 * i + 1;
        const right = 2 * i + 2;
        
        if (left < n && compare(arr, left, largest, steps) > 0) {
            largest = left;
        }
        
        if (right < n && compare(arr, right, largest, steps) > 0) {
            largest = right;
        }
        
        if (largest !== i) {
            swap(arr, i, largest, steps);
            heapify(n, largest);
        }
    }
    
    // Build max heap
    for (let i = Math.floor(n / 2) - 1; i >= 0; i--) {
        heapify(n, i);
    }
    
    // Extract elements from heap one by one
    for (let i = n - 1; i > 0; i--) {
        swap(arr, 0, i, steps);
        heapify(i, 0);
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Heap Sort algorithm object for export.
 */
const HeapSort = {
    name: 'Heap Sort',
    description: 'Builds a max heap structure and repeatedly extracts the maximum element. Efficient and consistent performance.',
    characteristics: {
        timeComplexity: 'O(n log n)',
        spaceComplexity: 'O(log n)',
        stable: false,
        bestFor: 'Guaranteed O(n log n) performance'
    },
    sort: heapSort
};

export { HeapSort };

/**
 * Shell Sort algorithm with step logging.
 * Generalization of insertion sort that allows exchange of far apart elements.
 * Uses gap sequence to progressively sort.
 * 
 * Time Complexity: O(n^(3/2)) to O(n^2) depending on gap sequence
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function shellSort(arr) {
    const steps = [];
    const n = arr.length;
    
    // Start with a large gap, then reduce the gap
    for (let gap = Math.floor(n / 2); gap > 0; gap = Math.floor(gap / 2)) {
        // Do a gapped insertion sort
        for (let i = gap; i < n; i++) {
            let j = i;
            
            // Shift earlier gap-sorted elements up until the correct location is found
            while (j >= gap) {
                if (compare(arr, j - gap, j, steps) > 0) {
                    swap(arr, j - gap, j, steps);
                    j -= gap;
                } else {
                    break;
                }
            }
        }
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Shell Sort algorithm object for export.
 */
const ShellSort = {
    name: 'Shell Sort',
    description: 'Improved insertion sort that compares elements far apart, then progressively reduces the gap. Creates interesting visual patterns.',
    characteristics: {
        timeComplexity: 'O(n^(3/2)) avg',
        spaceComplexity: 'O(1)',
        stable: false,
        bestFor: 'Medium-sized datasets'
    },
    sort: shellSort
};

export { ShellSort };

/**
 * Cocktail Shaker Sort (Bidirectional Bubble Sort) with step logging.
 * Variation of bubble sort that sorts in both directions alternately.
 * 
 * Time Complexity: O(n²)
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function cocktailShakerSort(arr) {
    const steps = [];
    const n = arr.length;
    let swapped = true;
    let start = 0;
    let end = n - 1;
    
    while (swapped) {
        swapped = false;
        
        // Forward pass (like bubble sort)
        for (let i = start; i < end; i++) {
            if (compare(arr, i, i + 1, steps) > 0) {
                swap(arr, i, i + 1, steps);
                swapped = true;
            }
        }
        
        if (!swapped) {
            break;
        }
        
        swapped = false;
        end--;
        
        // Backward pass
        for (let i = end - 1; i >= start; i--) {
            if (compare(arr, i, i + 1, steps) > 0) {
                swap(arr, i, i + 1, steps);
                swapped = true;
            }
        }
        
        start++;
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Cocktail Shaker Sort algorithm object for export.
 */
const CocktailShakerSort = {
    name: 'Cocktail Shaker Sort',
    description: 'Bidirectional bubble sort that alternates between forward and backward passes. Slightly more efficient than bubble sort.',
    characteristics: {
        timeComplexity: 'O(n²)',
        spaceComplexity: 'O(1)',
        stable: true,
        bestFor: 'Nearly sorted data with elements at both ends'
    },
    sort: cocktailShakerSort
};

export { CocktailShakerSort };

/**
 * Comb Sort algorithm with step logging.
 * Improvement over bubble sort that eliminates small values near the end (turtles).
 * Uses a shrinking gap to compare and swap distant elements.
 * 
 * Time Complexity: O(n²) worst case, O(n log n) average
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function combSort(arr) {
    const steps = [];
    const n = arr.length;
    let gap = n;
    const shrink = 1.3;
    let swapped = true;
    
    while (gap > 1 || swapped) {
        // Update gap for next comb
        gap = Math.floor(gap / shrink);
        if (gap < 1) {
            gap = 1;
        }
        
        swapped = false;
        
        // Compare and swap elements with current gap
        for (let i = 0; i + gap < n; i++) {
            if (compare(arr, i, i + gap, steps) > 0) {
                swap(arr, i, i + gap, steps);
                swapped = true;
            }
        }
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Comb Sort algorithm object for export.
 */
const CombSort = {
    name: 'Comb Sort',
    description: 'Improved bubble sort that eliminates small values near the end by using a shrinking gap. More efficient than bubble sort.',
    characteristics: {
        timeComplexity: 'O(n²) worst, O(n log n) avg',
        spaceComplexity: 'O(1)',
        stable: false,
        bestFor: 'General purpose, better than bubble sort'
    },
    sort: combSort
};

export { CombSort };

/**
 * Gnome Sort algorithm with step logging.
 * Simple sorting algorithm similar to insertion sort but moving elements
 * to their proper place by a series of swaps (like a gnome sorting garden pots).
 * 
 * Time Complexity: O(n²)
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function gnomeSort(arr) {
    const steps = [];
    const n = arr.length;
    let pos = 0;
    
    while (pos < n) {
        if (pos === 0) {
            pos++;
        } else {
            if (compare(arr, pos, pos - 1, steps) >= 0) {
                pos++;
            } else {
                swap(arr, pos, pos - 1, steps);
                pos--;
            }
        }
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Gnome Sort algorithm object for export.
 */
const GnomeSort = {
    name: 'Gnome Sort',
    description: 'Simple algorithm that works by moving elements to their proper place through swaps, like a gnome sorting garden pots.',
    characteristics: {
        timeComplexity: 'O(n²)',
        spaceComplexity: 'O(1)',
        stable: true,
        bestFor: 'Small datasets or educational purposes'
    },
    sort: gnomeSort
};

export { GnomeSort };

/**
 * Cycle Sort algorithm with step logging.
 * In-place sorting algorithm that minimizes writes by placing elements
 * in their correct positions through cycles.
 * 
 * Time Complexity: O(n²)
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function cycleSort(arr) {
    const steps = [];
    const n = arr.length;
    
    // Process each position
    for (let cycleStart = 0; cycleStart < n - 1; cycleStart++) {
        // Find the correct position for arr[cycleStart]
        let pos = cycleStart;
        
        for (let i = cycleStart + 1; i < n; i++) {
            if (compare(arr, i, cycleStart, steps) < 0) {
                pos++;
            }
        }
        
        // If element is already in correct position
        if (pos === cycleStart) {
            continue;
        }
        
        // Move element to its correct position through swaps
        while (pos !== cycleStart) {
            // Swap to move element closer to its position
            if (pos > cycleStart) {
                swap(arr, cycleStart, pos, steps);
            }
            
            // Recalculate position after swap
            pos = cycleStart;
            for (let i = cycleStart + 1; i < n; i++) {
                if (compare(arr, i, cycleStart, steps) < 0) {
                    pos++;
                }
            }
        }
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Cycle Sort algorithm object for export.
 */
const CycleSort = {
    name: 'Cycle Sort',
    description: 'Places elements in their correct positions through cycles. Minimizes the number of writes.',
    characteristics: {
        timeComplexity: 'O(n²)',
        spaceComplexity: 'O(1)',
        stable: false,
        bestFor: 'Minimizing write operations'
    },
    sort: cycleSort
};

export { CycleSort };

/**
 * Odd-Even Sort (Brick Sort) algorithm with step logging.
 * Parallel sorting algorithm based on bubble sort that compares
 * odd-indexed and even-indexed pairs alternately.
 * 
 * Time Complexity: O(n²)
 * Space Complexity: O(1)
 * 
 * @param {number[]} arr - Array to sort (modified in-place)
 * @returns {import('./types.js').SortingStep[]} Array of sorting steps
 */
function oddEvenSort(arr) {
    const steps = [];
    const n = arr.length;
    let sorted = false;
    
    while (!sorted) {
        sorted = true;
        
        // Odd phase
        for (let i = 1; i < n - 1; i += 2) {
            if (compare(arr, i, i + 1, steps) > 0) {
                swap(arr, i, i + 1, steps);
                sorted = false;
            }
        }
        
        // Even phase
        for (let i = 0; i < n - 1; i += 2) {
            if (compare(arr, i, i + 1, steps) > 0) {
                swap(arr, i, i + 1, steps);
                sorted = false;
            }
        }
    }
    
    steps.push({ type: 'complete' });
    return steps;
}

/**
 * Odd-Even Sort algorithm object for export.
 */
const OddEvenSort = {
    name: 'Odd-Even Sort',
    description: 'Parallel sorting algorithm that alternates between comparing odd and even indexed pairs. Creates distinctive visual patterns.',
    characteristics: {
        timeComplexity: 'O(n²)',
        spaceComplexity: 'O(1)',
        stable: true,
        bestFor: 'Parallel processing visualization'
    },
    sort: oddEvenSort
};

export { OddEvenSort };
