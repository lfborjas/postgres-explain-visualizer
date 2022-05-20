<template>
  <div id="app" class="d-flex">
    <pev2 :plan-source="plan" :plan-query="query"></pev2>
  </div>
</template>

<script>
import pev2 from "pev2";

const plan = window.plan || `
                                                           QUERY PLAN
---------------------------------------------------------------------------------------------------------------------------------
 Nested Loop  (cost=4.33..118.25 rows=10 width=488) (actual time=0.370..1.126 rows=10 loops=1)
   ->  Bitmap Heap Scan on tenk1 t1  (cost=4.33..39.44 rows=10 width=244) (actual time=0.254..0.380 rows=10 loops=1)
         Recheck Cond: (unique1 < 10)
         ->  Bitmap Index Scan on tenk1_unique1  (cost=0.00..4.33 rows=10 width=0) (actual time=0.164..0.164 rows=10 loops=1)
               Index Cond: (unique1 < 10)
   ->  Index Scan using tenk2_unique2 on tenk2 t2  (cost=0.00..7.87 rows=1 width=244) (actual time=0.041..0.048 rows=1 loops=10)
         Index Cond: (unique2 = t1.unique2)
 Total runtime: 2.414 ms`;

const query = window.query || `
SELECT *
FROM tenk1 t1, tenk2 t2
WHERE t1.unique1 < 10 AND t1.unique2 = t2.unique2;`;

export default {
  name: "App",
  data: function() {
    return {
      plan: plan,
      query: query
    };
  },
  components: {
    pev2
  }
};
</script>

<style>
html,
body,
#app {
  height: 100%;
}
</style>
