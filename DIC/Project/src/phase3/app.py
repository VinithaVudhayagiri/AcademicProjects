from flask import Flask, render_template, request
import datetime
import pickle
import pandas as pd
import matplotlib
import matplotlib.pyplot as plt

matplotlib.use('Agg')
app = Flask(__name__)
# Load the pickled machine learning model
model = pickle.load(open(r'model.pkl', 'rb'))
coefficients = model.coef_[0]

def generate_feature_importance_plot(application_df, coefficients):
    feature_names = application_df.columns.tolist()
    sorted_coefficients = sorted(zip(coefficients, feature_names), reverse=True)
    top_coefficients = sorted_coefficients[:10]
    top_features = [feature for coef, feature in top_coefficients]
    top_coefficients = [coef for coef, feature in top_coefficients]
    plt.figure(figsize=(6, 5))
    import numpy as np
    y_pos = np.arange(len(top_features))
    plt.barh(y_pos, top_features)
    plt.xlabel('Coefficient')
    plt.ylabel('Feature')
    plt.title('Top 10 Features Impacting Outcome')
    plt.xticks(rotation=90)
    plt.tight_layout()
    plt.savefig(r'static\images\feature_importance.png')

@app.route('/')
def index():
    return render_template('index.html')

@app.route('/apply', methods=['GET', 'POST'])
def apply():
    if request.method == 'POST':

        GENDER_CONVERSION = {'female': 0, 'male': 1}
        BINARY_CONVERSION = {'no': 0, 'yes': 1}
        NAME_INCOME_TYPE_CONVERSION = {'commercial_associate': 0, 'pensioner': 1, 'state_servant': 2, 'student': 3, 'working': 4}
        NAME_FAMILY_STATUS_CONVERSION = {'civil-marriage': 0, 'married': 1, 'separated': 2, 'single': 3, 'widow': 4}
        OCCUPATION_TYPE_CONVERSION = {'accountants': 0, 'cleaning_staff': 1, 'cooking_staff': 2, 'core_staff': 3, 'drivers': 4, 'hr_staff': 5, 'high_skill_tech_staff': 6, 'it_staff': 7, 'laborers': 8, 'low-skill_laborers': 9, 'managers': 10, 'medicine_staff': 11, 'private_service_staff': 12, 'realty_agents': 13, 'sales_staff': 14, 'secretaries': 15, 'security_staff': 16, 'waiters/barmen_staff': 17,'no_job':18}
        NAME_HOUSING_TYPE_CONVERSION = {'co-op-apartment': 0, 'house-apartment': 1, 'municipal-apartment': 2, 'office-apartment': 3, 'rented-apartment': 4, 'with-parents': 5}
        NAME_EDUCATION_TYPE_CONVERSION = {'academic_degree': 0, 'higher_education': 1, 'incomplete_higher': 2, 'lower_secondary': 3, 'secondary_secondary_special': 4}

        name = request.form['name']
        gender = GENDER_CONVERSION[request.form['gender'].lower()]
        own_car = BINARY_CONVERSION[request.form['own_car'].lower()]
        own_realty = BINARY_CONVERSION[request.form['own_realty'].lower()]
        cnt_children = request.form['cnt-children']
        total_income = request.form['total_income']
        income_type = NAME_INCOME_TYPE_CONVERSION[request.form['income_type'].lower()]
        education_type = NAME_EDUCATION_TYPE_CONVERSION[request.form['education_type'].lower()]
        marital_status = NAME_FAMILY_STATUS_CONVERSION[request.form['marital-status'].lower()]
        housing_type = NAME_HOUSING_TYPE_CONVERSION[request.form['housing-type'].lower()]
        days_birth = (datetime.datetime.now().date() - datetime.datetime.strptime(request.form['dob'],'%Y-%m-%d').date()).days
        days_employed = (datetime.datetime.now().date() - datetime.datetime.strptime(request.form['employed_date'],'%Y-%m-%d').date()).days
        work_phone = BINARY_CONVERSION[request.form['work-mobile'].lower()]
        personal_mobile = BINARY_CONVERSION[request.form['personal-mobile'].lower()]
        email = BINARY_CONVERSION[request.form['email'].lower()]
        occupation_type = OCCUPATION_TYPE_CONVERSION[request.form['occupation_type'].lower()]
        family_size = request.form['family-count']
        age = days_birth/365
        total_experience = days_employed/365

        application_data = {
            'CODE_GENDER': gender,
            'FLAG_OWN_CAR': own_car,
            'FLAG_OWN_REALTY': own_realty,
            'CNT_CHILDREN': cnt_children,
            'AMT_INCOME_TOTAL': total_income,
            'NAME_INCOME_TYPE': income_type,
            'NAME_EDUCATION_TYPE': education_type,
            'NAME_FAMILY_STATUS': marital_status,
            'NAME_HOUSING_TYPE': housing_type,
            'DAYS_BIRTH': days_birth,
            'DAYS_EMPLOYED': days_employed,
            'FLAG_WORK_PHONE': work_phone,
            'FLAG_PHONE': personal_mobile,
            'FLAG_EMAIL': email,
            'OCCUPATION_TYPE': occupation_type,
            'CNT_FAM_MEMBERS': family_size,
            'AGE': float(age),
            'TOTAL_EXPERIENCE': float(total_experience)
        }
        application_df = pd.DataFrame.from_dict([application_data])
        outcome = model.predict(application_df)[0]

        # Visualization
        generate_feature_importance_plot(application_df, coefficients)

        if outcome:
            return render_template('application_approved.html')
        else:
            return render_template('application_rejected.html')

    else:
        return render_template('apply.html')


@app.route('/status')
def status():
    status = 'pending' # replace with actual status
    return render_template('status.html', status=status)


if __name__ == '__main__':
    app.run()
