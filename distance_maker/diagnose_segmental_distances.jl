"""
Diagnostic: show which natural classes two segments share and don't share.
"""
function diagnose_similarity(seg1::String, seg2::String, 
                             inventory::SegmentInventory,
                             classes::Vector{NaturalClass})
    println("\n=== Comparing $seg1 and $seg2 ===")
    
    # Show feature specifications
    idx1 = findfirst(==(seg1), inventory.segments)
    idx2 = findfirst(==(seg2), inventory.segments)
    
    println("\nFeature specifications:")
    println("Feature\t\t$seg1\t$seg2")
    for (i, feat) in enumerate(inventory.features)
        val1 = inventory.matrix[idx1, i]
        val2 = inventory.matrix[idx2, i]
        println("$feat\t\t$(ismissing(val1) ? "∅" : val1)\t$(ismissing(val2) ? "∅" : val2)")
    end
    
    shared = NaturalClass[]
    non_shared_1 = NaturalClass[]
    non_shared_2 = NaturalClass[]
    
    for nc in classes
        in_seg1 = seg1 ∈ nc.members
        in_seg2 = seg2 ∈ nc.members
        
        if in_seg1 && in_seg2
            push!(shared, nc)
        elseif in_seg1
            push!(non_shared_1, nc)
        elseif in_seg2
            push!(non_shared_2, nc)
        end
    end
    
    println("\nShared classes ($(length(shared))):")
    for nc in shared[1:min(10, length(shared))]  # show first 10
        print_natural_class(nc)
    end
    if length(shared) > 10
        println("... and $(length(shared) - 10) more")
    end
    
    println("\nClasses containing only $seg1 ($(length(non_shared_1))):")
    for nc in non_shared_1[1:min(5, length(non_shared_1))]
        print_natural_class(nc)
    end
    if length(non_shared_1) > 5
        println("... and $(length(non_shared_1) - 5) more")
    end
    
    println("\nClasses containing only $seg2 ($(length(non_shared_2))):")
    for nc in non_shared_2[1:min(5, length(non_shared_2))]
        print_natural_class(nc)
    end
    if length(non_shared_2) > 5
        println("... and $(length(non_shared_2) - 5) more")
    end
    
    sim = length(shared) / (length(shared) + length(non_shared_1) + length(non_shared_2))
    println("\nSimilarity: $sim")
end

# Diagnose some problematic pairs
diagnose_similarity("z", "š", inventory, classes)
diagnose_similarity("t", "d", inventory, classes)
diagnose_similarity("p", "b", inventory, classes)