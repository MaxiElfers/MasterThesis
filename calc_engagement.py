import pandas as pd
import os

# Folder with individual participant .xlsx files
input_folder = 'participant_files'
output_rows = []

# Engagement weights
w1, w2 = 1.2, 0.8

# Go through each participant file
for filename in os.listdir(input_folder):
    if filename.endswith('.xlsx'):
        file_path = os.path.join(input_folder, filename)
        df = pd.read_excel(file_path)

        participant_id = os.path.splitext(filename)[0]
        row_data = {'participant': participant_id}
        engagement_sum = 0
        room_count = 0

        for _, row in df.iterrows():
            room = row['Room']

            # Extract and convert time values
            try:
                total = float(str(row['overall_time (in sec)']).replace(',', '.'))
                wl = float(str(row['walking_and_looking (sec)']).replace(',', '.'))
                wnl = float(str(row['walking_and_not_looking (sec)']).replace(',', '.'))
                sl = float(str(row['standing_and_looking (sec)']).replace(',', '.'))
                snl = float(str(row['standing_and_not_looking (sec)']).replace(',', '.'))
            except:
                total, wl, wnl, sl, snl = 0, 0, 0, 0, 0

            # Calculate engagement
            if total > 0:
                engagement = (((w1 * sl) + (w2 * wl)) / total)
            else:
                engagement = 0

            # Store values per room
            row_data[f'{room}_engagement'] = round(engagement, 4)
            row_data[f'{room}_overall_time'] = total
            row_data[f'{room}_walking_and_looking'] = wl
            row_data[f'{room}_walking_and_not_looking'] = wnl
            row_data[f'{room}_standing_and_looking'] = sl
            row_data[f'{room}_standing_and_not_looking'] = snl

            engagement_sum += engagement
            room_count += 1

        row_data['overall_engagement'] = round(engagement_sum / room_count, 4) if room_count > 0 else 0
        output_rows.append(row_data)

# Create final DataFrame
summary_df = pd.DataFrame(output_rows)

# Save to Excel
summary_df.to_excel('engagement_summary_detailed.xlsx', index=False)
print("✅ Saved to engagement_summary_detailed.xlsx")
