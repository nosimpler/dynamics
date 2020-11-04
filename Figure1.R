library(DiagrammeR)

grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle]        
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab5 [label = '@@5']

      # edge definitions with the node IDs
      tab1 -> tab2;
      tab2 -> tab3;
      tab3 -> tab4;
      }

      [1]: 'Cool-start NMF on individual sessions (4 factors per session)'
      [2]: 'Cool-start NMF on (FR x (INDIV x 4))
      (7 x 694 frequency factors) -> 7 Group frequency factors'
      [3]: 'Warm-start NMF on individual sessions using Group frequency factors'
      ")