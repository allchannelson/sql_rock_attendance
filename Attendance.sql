-- Reserved Dynamic Data Block parameters (do not use these variables in the SQL)
-- Parameters: inputgroupid=58831

-- Selection URL: ~/GroupMember/{gmId}
-- adapted code from https://stackoverflow.com/a/20936310/3715973

-- Page Title Lava: {{ 'Global' | PageParameter:'inputtitle' }}

/*
a.startdatetime has seemingly redundant conversion conversion: datetime -> varchar -> datetime -> varchar
The extra initial datetime -> varchar was done due to distinct issues in the @colList variables; the same date but different time will cause the distinct not group them together, so at that stage, I converted it to varchar using format 107 (mm/dd/yyyy).  However, the varchar column caused problems in existing code due to it expecting datetime, so I converted it back to datetime. This drops the time portion of a datetime datatype, making it work as required.
*/

Declare @colList varchar(max)
Declare @colListBit varchar(max)
Declare @colListCount varchar(max)
Declare @qry varchar(max)
Declare @firstattendancedate varchar(max)
Declare @sanitizedgroupid int

-- DEBUG PARAMETERS
-- Declare @inputgroupid varchar(max); SET @inputgroupid = '59098';
-- Declare @inputgroupid varchar(max); SET @inputgroupid = '57311';

Declare @attrList varchar(max) = ''
Declare @attrListCol varchar(max)

SET @sanitizedgroupid = convert(int, @inputgroupid)

SET @firstattendancedate = (select distinct min(CONVERT(varchar, a.startdatetime, 107)) from "attendanceoccurrence" ao
    join "attendance" a on ao.id = a.occurrenceid
    where ao.groupid = @sanitizedgroupid)

SET @colList = STUFF((
    SELECT ',' + QUOTENAME(CONVERT(varchar, startdatetime, 107)) 
        FROM (select distinct convert(datetime, convert(varchar, a.startdatetime, 101)) "startdatetime" from "group" g
        join "attendanceoccurrence" ao on g.id = ao.groupid
        join "attendance" a on ao.id = a.occurrenceid
        where g.id = @sanitizedgroupid
        and didattend = 1) t
        order by startdatetime desc
        FOR XML PATH(''), TYPE
    ).value('.', 'NVARCHAR(MAX)') 
    ,1,1,'')

SET @colListCount = STUFF((
    SELECT ', ''<a href="https://rock.rocksolidchurchdemo.com/page/364?GroupId=' + convert(varchar, @sanitizedgroupid) + '&OccurrenceId=' + occurrenceid + '">'' + CONVERT(VARCHAR, SUM(ISNULL(' + QUOTENAME(CONVERT(varchar, startdatetime, 107)) + ',0))) + ''</a>''' + QUOTENAME(CONVERT(varchar, startdatetime, 107))

        FROM (select distinct convert(datetime, convert(varchar, a.startdatetime, 101)) "startdatetime", convert(varchar, occurrenceid) "occurrenceid" from "group" g
        join "attendanceoccurrence" ao on g.id = ao.groupid
        join "attendance" a on ao.id = a.occurrenceid
        where g.id = @sanitizedgroupid
        and didattend = 1) t
        order by startdatetime desc
        FOR XML PATH(''), TYPE
    ).value('.', 'NVARCHAR(MAX)') 
    ,1,1,'')

SET @colListBit = (
    SELECT ', case when convert(bit, ' + QUOTENAME(CONVERT(varchar, startdatetime, 107))  + ') = 0 THEN NULL ELSE convert(bit, ' + QUOTENAME(CONVERT(varchar, startdatetime, 107))  + ') END as ' + QUOTENAME(CONVERT(varchar, startdatetime, 107))
    -- SELECT distinct ', convert(bit, ' + QUOTENAME(CONVERT(varchar, a.startdatetime, 107))  + ') as ' + QUOTENAME(CONVERT(varchar, a.startdatetime, 107))
        FROM (select distinct convert(datetime, convert(varchar, a.startdatetime, 101)) "startdatetime" from "group" g
        join "attendanceoccurrence" ao on g.id = ao.groupid
        join "attendance" a on ao.id = a.occurrenceid
        where g.id = @sanitizedgroupid
        and didattend = 1) t
        order by startdatetime desc
        FOR XML PATH(''), TYPE
    ).value('.', 'NVARCHAR(MAX)');


-- special condition for classes that have not started yet (no attendance taken)
SET @colList = (select (case when isnull(@colList, '') = '' then QUOTENAME('Not Started') else @colList end))
SET @colListBit = (select (case when isnull(@colListBit, '') = '' then concat(', ', QUOTENAME('Not Started')) else @colListBit end))
SET @firstattendancedate = (select (case when isnull(@firstattendancedate, '') = '' then QUOTENAME('Not Started') else @firstattendancedate end))

-- this version of the variable population method is easier to read due to lack of STUFF and FOR XML PATH, but requires a separate LEFT to trim, the output is comma delimited column names
select @attrList += quotename(attrAll."key") + ',' from "group" g
    join "attribute" attrAll on
    attrAll.EntityTypeQualifierColumn = 'GroupId' and g.id = attrAll.EntityTypeQualifierValue
    -- and convert(varchar, g.grouptypeid) = attr.entitytypequalifiervalue 
    -- and attr."EntityTypeQualifierColumn" = 'GroupTypeId'
    where g.id = @sanitizedgroupid
    order by attrAll.[order]

SET @attrList = CASE WHEN (LEN(@attrList) > 0) THEN LEFT(@attrList, LEN(@attrList) - 1) ELSE '' END
SET @attrListCol = CASE WHEN (LEN(@attrList) > 0) THEN CONCAT(', ', @attrList) ELSE '' END

-- Note that @colListBit and @attrList has a leading comma but @colList don't, it is so that an empty list will not cause an error

SET @qry = '
select '+@colListCount+'
from (
SELECT *
FROM (
  select distinct pa.personid ''pId'', gm.id ''gmId'', p.email, isnull(attrv_walkin.valueasboolean, 0) ''Walk-in'', CONVERT(varchar, a.startdatetime, 107) ''attend_date'', cast(a.didattend as integer) ''didattend''
  from "group" g
  left join "attendanceoccurrence" ao on g.id = ao.groupid
  left join "attendance" a on ao.id = a.occurrenceid
  join "personalias" pa on a.personaliasid = pa.id
  join "person" p on pa.personid = p.id
  left join "groupmember" gm on gm.groupid = g.id and gm.personid = pa.personid
  left join "attribute" attr_walkin on g.grouptypeid = attr_walkin.entitytypequalifiervalue and attr_walkin."key" = ''Walk-in'' and attr_walkin."EntityTypeQualifierColumn" = ''GroupTypeId''
  left join "attributevalue" attrv_walkin on attrv_walkin.attributeid = attr_walkin.id and attrv_walkin.entityid = gm.id
  where g.id = '+convert(varchar,@sanitizedgroupid)+' and gm.isarchived = 0
  
  union
  
  select distinct p.id ''pId'', gm.id ''gmId'', p.email, isnull(attrv_walkin.valueasboolean, 0) ''Walk-in'', '''+@firstattendancedate+''' ''attend_date'',
  '''' ''didattend''
  from "group" g
  join "groupmember" gm on gm.groupid = g.id 
  join "person" p on gm.personid = p.id
  left join "attribute" attr_walkin on g.grouptypeid = attr_walkin.entitytypequalifiervalue and attr_walkin."key" = ''Walk-in'' and attr_walkin."EntityTypeQualifierColumn" = ''GroupTypeId''
  left join "attributevalue" attrv_walkin on attrv_walkin.attributeid = attr_walkin.id and attrv_walkin.entityid = gm.id
  where g.id = '+convert(varchar,@sanitizedgroupid)+' and gm.isarchived = 0
  and p.id not in (select distinct pa.personid from "attendanceoccurrence" ao
    join "attendance" a on ao.id = a.occurrenceid
    join "personalias" pa on a.personaliasid = pa.id
    where ao.groupid = '+convert(varchar,@sanitizedgroupid)+'
    )
) as s
PIVOT
(
  MAX(didattend)
  FOR attend_date IN (' + @colList + ')
) pvt) temp
JOIN "person" p on temp.pid = p.id'

-- conditional @qry building because if @attrList is empty, the PIVOT FOR() will error due to an empty list
IF LEN(@attrList) > 0
BEGIN
  SET @qry = @qry + '
LEFT JOIN (
  select * from (
  select p.id, attrAll."key" "AttrName", REPLACE(attrAllV.value, ''|'', '''') "AttrValue" from "group" g
  join "groupmember" gm on g.id = gm.groupid
  join "person" p on gm.personid = p.id
  join "attribute" attrAll on attrAll.EntityTypeQualifierColumn = ''GroupId'' and g.id = attrAll.EntityTypeQualifierValue
  join "attributevalue" attrAllV on attrAllV.attributeid = attrAll.id and attrAllV.entityid = gm.id

  where g.id = '+convert(varchar,@sanitizedgroupid)+'
  ) as attrTemp
  PIVOT
  (
      MAX(AttrValue)
      FOR AttrName IN (' + @attrList + ')
  ) as attrPivot
) attrPivotT ON attrPivotT.id = p.id
'
END -- Conditional @qry building ENDS

-- SET @qry = @qry + ' order by concat(upper(p.lastname), '', '', p.firstname, isnull(concat('' '',p.middlename), '''')) asc'

EXEC(@qry)
-- select @qry
-- select @colList
-- select @colListCount
-- select @colListBit
-- select @attrList
-- select @attrListCol
