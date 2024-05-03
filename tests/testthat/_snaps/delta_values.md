# delta_values() wrapper works

    Code
      test_wrapper
    Output
      # A tibble: 1,080 x 98
         file_id   cycle Analysis    r44    r45    r46    r47   r48   r49   r54    s44
         <chr>     <int> <chr>     <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl>
       1 180814_7~     1 4841     16527. 19533. 22905. 25478. 2109. -266. -268. 16368.
       2 180814_7~     2 4841     16280. 19241. 22562. 25096. 2078. -261. -263. 16087.
       3 180814_7~     3 4841     16022. 18937. 22205. 24701. 2046. -257. -259. 15811.
       4 180814_7~     4 4841     15761. 18629. 21844. 24301. 2013. -252. -255. 15540.
       5 180814_7~     5 4841     15507. 18329. 21493. 23909. 1980. -248. -250. 15275.
       6 180814_7~     6 4841     15259. 18036. 21149. 23527. 1949. -244. -246. 15016.
       7 180814_7~     7 4841     15014. 17746. 20810. 23148. 1918. -239. -242. 14762.
       8 180814_7~     8 4841     14774. 17463. 20477. 22780. 1888. -236. -238. 14513.
       9 180814_7~     9 4841     14539. 17185. 20152. 22417. 1858. -232. -234. 14269.
      10 180814_7~    10 4841     14309. 16913. 19833. 22062. 1829. -228. -230. 14030.
      # i 1,070 more rows
      # i 87 more variables: s45 <dbl>, s46 <dbl>, s47 <dbl>, s48 <dbl>, s49 <dbl>,
      #   s54 <dbl>, outlier_cycle_low_r44 <lgl>, outlier_cycle_low_s44 <lgl>,
      #   outlier_cycle_high_r44 <lgl>, outlier_cycle_high_s44 <lgl>,
      #   cycle_diff_r44 <dbl>, cycle_diff_s44 <dbl>, cycle_drop_r44 <lgl>,
      #   cycle_drop_s44 <lgl>, cycle_drop_num_r44 <int>, cycle_drop_num_s44 <int>,
      #   outlier_cycle_drop_r44 <lgl>, outlier_cycle_drop_s44 <lgl>, ...

