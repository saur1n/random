# sql query for creating a smudgebox

```
insert into <tablename_smudgebox>
select pos
from <tablename_pos2coor>
where col between <n1> and <n2>
and row between <n3> and <n4>
#and pos > <n5>
order by plate, col, row
```
