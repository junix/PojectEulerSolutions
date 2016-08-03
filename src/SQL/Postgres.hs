module SQL.Postgres where
import Database.HDBC
import Database.HDBC.PostgreSQL

conn :: IO ()
conn = do
    cn <- connectPostgreSQL "host=192.168.99.100 dbname=hello user=postgres"
    dset <- quickQuery' cn "select * from path" []
    print dset
    disconnect cn
    print "he"

