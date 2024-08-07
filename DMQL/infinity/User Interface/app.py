from flask import Flask, render_template, request
import psycopg2

app = Flask(__name__)

# Connect to the PostgreSQL database
try:
    conn = psycopg2.connect(dbname="SemesterProject", user="postgres", password="postgres")
    print("connected")
except Exception as e:
    raise Exception("Connection not successful {e}")

# Get the list of tables
cursor = conn.cursor()
tables = ['customer', 'employee', 'fact_sales_daily', 'orders','product', 'sale', 'seller', 'store']

@app.route('/')
def home():
    return render_template('home.html', tables=tables)

@app.route('/choose_option', methods=['POST'])
def choose_option():
    table = request.form['table']
    if not table:
        return render_template('home.html', tables=tables, error_message="Please select a table.")
    elif table not in tables:
        return render_template('home.html', tables=tables, error_message="Invalid table selected.")

    # Query the table to display in the view.html template
    cursor.execute(f"SELECT * FROM {table} limit 20")
    headers = [desc[0] for desc in cursor.description]

    if request.form['option'] == 'insert':
        return render_template('insert.html', table=table, headers=headers)
    elif request.form['option'] == 'update':
        return render_template('update.html', table=table, headers=headers, num_cols=len(headers))
    elif request.form['option'] == 'view':
        rows = cursor.fetchall()
        return render_template('view.html', table=table, headers=headers, rows=rows)
    elif request.form['option'] == 'delete':
        cursor.execute(f"SELECT * FROM {table}")
        headers = [desc[0] for desc in cursor.description]
        if table.lower() == 'sale':
            col = headers[1]
        else:
            col = headers[0]
        cursor.execute(f"SELECT {col} FROM {table} order by 1")
        rows = cursor.fetchall()
        return render_template('delete.html', table=table, columns=col, records=rows)


@app.route('/insert_record', methods=['POST'])
def insert_record():
    table = request.form['table']
    headers = request.form.getlist('headers[]')
    values = request.form.getlist('values[]')

    # Construct the SQL query
    query = f"INSERT INTO {table} ({', '.join(headers)}) VALUES ({', '.join(['%s']*len(values))})"

    try:
        # Execute the SQL query
        cursor.execute(query, values)
        conn.commit()
        return render_template('home.html', tables=tables, success_message="Record inserted successfully.")
    except Exception as e:
        return render_template('insert.html', table=table, headers=headers, error_message=f"Error: {e}")

@app.route('/update_record', methods=['POST'])
def update_record():
    print("did you get the control?")
    table = request.form['table']
    print("may be")
    num_cols = int(request.form['num_cols'])
    print("yes I did")

    # Construct the SET clause of the SQL query
    set_clause = ', '.join([f"{request.form[f'col{i}']} = %s" for i in range(num_cols)])

    # Construct the WHERE clause of the SQL query
    condition = request.form['condition']

    # Construct the full SQL query
    query = f"UPDATE {table} SET {set_clause} WHERE {condition}"
    print(query)
    try:
        # Execute the SQL query
        values = [request.form[f'val{i}'] for i in range(num_cols)]
        cursor.execute(query, values)
        conn.commit()
        return render_template('home.html', tables=tables, success_message="Record updated successfully.")
    except Exception as e:
        cursor.rollback()
        headers = request.form.getlist('headers')
        return render_template('update.html', table=table, headers=headers, num_cols=num_cols,
                               error_message=f"Error: {e}")


@app.route('/delete_record', methods=['POST'])
def delete_record():
    table = request.form['table']
    condition_column = request.form['column']
    condition_value = request.form['value']

    # Construct the SQL query
    query = f"DELETE FROM {table} WHERE {condition_column}='{condition_value}'"
    try:
        # Execute the SQL query
        cursor.execute(query)
        conn.commit()
        return render_template('home.html', tables=tables, success_message="Record deleted successfully.")
    except Exception as e:
        # Query the table to display in the delete.html template
        cursor.execute(f"SELECT * FROM {table} limit 20")
        rows = cursor.fetchall()
        headers = [desc[0] for desc in cursor.description]
        return render_template('delete.html', table=table, columns=headers, records=rows, error_message=f"Error: {e}")





if __name__ == '__main__':
    app.run()

